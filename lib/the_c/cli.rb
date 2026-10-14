# frozen_string_literal: true
#
# Copyright 2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

require "fileutils"
require "shellwords"

module TheC
  ## Main Ruby entry points for bashrc code to use. This manages named pipes, listens for input, and responds the
   # output. This is designed to be a background process that runs alongside the bash process. With this design the size
   # of the code does not matter, and memoizing can be leveraged, and responses are as fast as possible.
   #
  class Cli
    include TheC::Mixin::Info
    include TheC::Mixin::Util

    attr :pipe_path_i, :pipe_path_o

    def initialize(args)
      case args
      in ["bash_init"]         then bash_init
      in ["server"]            then run_server
      in ["client", /\A\d+\z/] then add_client(args[1])
      else raise "Invalid args"
      end
    end

    ## Emit bash initialization code, including the main "c" function, to a temp file, and output the bash command to
     # source the file.
     #
    def bash_init
      ensure_dirs
      script = File.binread("#{__dir__}/bash_init.bash")
      script.gsub!(/<%=.+?%>/) { |match| eval(match[3..-3]) }
      File.binwrite(init_file, script)
      puts ". #{init_file.shellescape}"
    end

    ## Initialize the service and enter event loop if not already running.
     #
    def run_server
      lock(:acquire)
      begin
        return if File.readable?(pid_file)
        File.binwrite(pid_file, PID_S)
      ensure
        lock(:release)
      end
      FileUtils.rm_f(init_file) # From `bash_init`
      adjust_process
      clean_old_pipes
      #install_new_pipes
      RubyVM::YJIT.enable rescue puterr "++ Could not enable YJIT."
      server_loop # No return until exit/sigterm
      clean_area
    end

    def add_client(client_pid)
      nil
    end

    def ensure_dirs
      wbits, ubits = 0o1777, 0o0700
      if Dir.exist?(work_root)
        File.chmod(wbits, work_root) if File.stat(work_root).mode & wbits != wbits
      else
        FileUtils.mkdir_p(work_root, mode: wbits)
      end
      FileUtils.mkdir_p(base_path,   mode: ubits)
      FileUtils.mkdir_p(server_area, mode: ubits)
      FileUtils.mkdir_p(client_area, mode: ubits)
    end

    ## Use filesystem dir as a mutex for synchronizing critical sections across multiple processes.
     #
    def lock(op)
      case op
      when :acquire
        until nil
          begin
            Dir.mkdir("#{base_path}/lock")
            break
          rescue Errno::EEXIST
            sleep rand(0.05 .. 0.10)
          end
        end
      when :release
        Dir.rmdir("#{base_path}/lock")
      end
    end

    def server_loop
      begin
        shortcuts = TheC::Shortcuts::Core.new
        while (line = line_input)
          result = shortcuts.c(*line.shellsplit)
          line_output(result)
        end
      rescue Exception => e
        return if SystemExit === e
        return if SignalException === e && e.signo == 15 # SIGTERM
        info = e.backtrace&.[](0)
        puterr "++ server_loop: Restarting after #{e.class}: #{e.message}: #{info}"
        sleep 1
        retry
      end
    end

    def clean_area
      File.delete(pid_file)
    end

    def line_input = File.open(pipe_path_i, "r") { _1.gets }

    def line_output(line) = File.open(pipe_path_o, "a") { _1.puts(line) }

    def install_new_pipes
      @pipe_path_i, @pipe_path_o = in_out_delims.map { "#{base_path}#{_1}#{my_pid}" }
      [pipe_path_i, pipe_path_o].each { File.mkfifo(_1, 0o600) }
    end

    def clean_old_pipes
      # Try removing old pipes that somehow got left open
      in_out_delims.each do |delim|
        Dir.glob("#{base_path}#{delim}*").each do |check_path|
          begin
            next if Process.uid != File.stat(check_path).uid
          rescue Errno::ENOENT
            next # A racing process probably removed it
          end
          check_pid = check_path.split(delim).last
          clean_it = (check_pid == my_pid)
          if ! clean_it
            check_cmd = `ps -o command= -p #{check_pid}`
            clean_it = ! check_cmd.start_with?(proctitle_base)
          end
          if clean_it
            begin
              File.delete(check_path)
            rescue => e
              next if Errno::ENOENT === e # A racing process probably removed it
              puterr "+ Delete #{check_path.inspect} failed: #{e.class}: #{e.message}"
            end
          end
        end
      end
    end

    def adjust_process
      #Process.setpgrp # Use this if signals from bash mess up this process
      Process.setproctitle("#{proctitle_base} #{ppid}")
    end

    def proctitle_base = "the_c-ruby #{base_path.shellescape}"
  end
end
