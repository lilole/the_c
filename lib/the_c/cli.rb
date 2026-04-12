# frozen_string_literal: true

module TheC
  ## Main Ruby entry points for bashrc code to use. This manages named pipes,
   # listens for input, and responds the output. This is designed to be a
   # background process that runs alongside the bash process. With this design
   # the size of the code does not matter, and memoizing can be leveraged,
   # and responses are as fast as possible.
   #
  class Cli
    include TheC::Mixin::Util

    attr_reader :base_path, :in_out_delims, :my_pid, :pipe_path_i, :pipe_path_o, :ppid

    def initialize(args)
      @base_path = "/tmp/#{File.basename($0)}"
      if args[0] == "bash_init"
        bash_init
      elsif args.size == 1
        @ppid = args[0].to_i
        @my_pid = Process.pid
        @in_out_delims = %w[-i -o] # Separate the base filename from pid for each pipe
        start
      else
        raise "Invalid args"
      end
    end

    ## Emit bash initialization code, including the main "c" function, to a temp
     # file, and return the bash command to source the file.
     #
    def bash_init
      script = +(<<~'END')
        ## Set up this bash instance and the Ruby background command service to
         # interface with each other from the `c` func.
         #
        the_c() {
          declare -gA THE_C
          while [[ $1 ]]; do
            local _sub_cmd="$1"; shift
            case "$_sub_cmd" in
              init)
                [[ ${THE_C[path]} ]] || THE_C[path]="<%= $0 %>"
                [[ ${THE_C[base]} ]] || THE_C[base]="<%= base_path %>"
                THE_C[tmp]=$(for d in /dev/shm /tmp; do [[ -w $d ]] && { echo $d; break; }; done)
                the_c assert tmp || return 2
                THE_C[lock]="${THE_C[tmp]}/the_c-$$.lock"
              ;;

              assert)
                local attr attrs msg quiet=false
                [[ $1 ]] && { attrs="$*"; set --; } || attrs='-q pid' # Must consume args
                for attr in $attrs; do
                  [[ $attr == -q ]] && { quiet=true; continue; }
                  [[ ${THE_C[$attr]} ]] && continue
                  if ! $quiet; then
                    case "$attr" in
                      tmp)  msg='+ the_c: Cannot init: Cannot find tmp dir.' ;;
                      lock) msg='+ the_c: Call init first.' ;;
                      pid)  msg='+ the_c is not running.' ;;
                      *)    msg="+ the_c: Attribute '$attr' is not set." ;;
                    esac
                    echo >&2 "$msg"
                  fi
                  return 1
                done
              ;;

              start)
                the_c assert lock || return 2
                "${THE_C[path]}" $$ < /dev/null & # Start the bg service
                THE_C[pid]=$!
                THE_C[input]="${THE_C[base]}-i${THE_C[pid]}"  # Must match named pipe in the service
                THE_C[output]="${THE_C[base]}-o${THE_C[pid]}" # Must match named pipe in the service
                trap 'the_c stop' EXIT
                local t=30; while [[ ! -e ${THE_C[output]} ]]; do (( --t < 1 )) && break; sleep 0.1; done
                (( t > 0 )) || { echo >&2 '+ the_c: Did not start.'; the_c stop; return 2; }
              ;;

              stop)
                local rc=0
                the_c assert pid && kill "${THE_C[pid]}";    (( rc += $? ))
                rm -f "${THE_C[input]}" "${THE_C[output]}";  (( rc += $? ))
                unset THE_C[pid] THE_C[input] THE_C[output]; (( rc += $? ))
                (( rc == 0 )) || sleep 3
              ;;

              status)
                {
                  echo -e '\nProcesses:'; the_c assert pid; c psg '\b(pts/\d+|bash|the_c)\b'
                  echo -e '\nPipes:';     the_c assert base && c l -t "${THE_C[base]}"*
                  echo -e '\nTmpFiles:';  the_c assert tmp  && c l -t "${THE_C[tmp]}"/TheC-Shortcuts-*
                } 2>&1 | c m
              ;;

              lock_on)
                the_c assert lock && while ! mkdir "${THE_C[lock]}" &> /dev/null; do sleep 0.1; done
              ;;

              lock_off)
                the_c assert lock && rmdir "${THE_C[lock]}"
              ;;

              *) return 1 ;;
            esac
          done
        }

        ## The wrapper for all the Ruby shortcuts.
         #
        c() {
          local last_rc=$? # Must be first; may be displayed in PS1
          the_c assert pid || return 1

          # Send the command to the bg service.
          # Note that caching `$last_rc` or `pwd` sometimes breaks here, so we always send them.
          local -a cmd=(
            "{{ENV[\"THE_C_LAST_RC\"]=\"$last_rc\";Dir.chdir(\"${PWD//\"/\\\"}\")}}"
            "$@"
          )
          the_c lock_on
          echo "${cmd[*]@Q}" > "${THE_C[input]}"

          # Read the response from the bg service
          local result
          IFS='' read -r result < "${THE_C[output]}"
          the_c lock_off

          # Complete the bg service logic in this context
          eval "$result"
        }

        the_c assert || the_c init start
      END

      script.gsub!(/<%=.+?%>/) { |match| eval(match[3..-3].strip) }
      script_file = "#{base_path}.init.#{$$}"
      File.write(script_file, script)
      puts ". #{script_file}"
    end

    ## Initialize the service and enter event loop.
     #
    def start
      TheC::Mixin::TempFile.add_to_clean(0, "#{base_path}.init.*") # For `bash_init()`
      adjust_process
      clean_old_pipes
      install_new_pipes
      event_loop
    end

    def event_loop
      RubyVM::YJIT.enable rescue puterr "++ Could not enable YJIT."
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
        puterr "++ event_loop: Restarting after #{e.class}: #{e.message}: #{info}"
        sleep 1
        retry
      end
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
          check_pid = check_path.split(delim).last.to_i
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
