# frozen_string_literal: true

module TheC
module Mixin
  ## Generic methods for system or context info.
   #
  module Info
    def base_path     = @base_path     ||= "#{work_root}/#{my_uid}"
    def client_area   = @client_area   ||= "#{base_path}/clients/#{my_pid}"
    def cpu_count     = @cpu_count     ||= IO.read("/proc/cpuinfo").scan(/^processor/).size
    def in_out_delims = @in_out_delims ||= %w[-i -o] # Separate the base filename from pid for each pipe
    def init_file     = @init_file     ||= "#{base_path}/init_#{PID_S}"
    def my_pid        = @my_pid        ||= Process.pid.to_s
    def my_uid        = @my_uid        ||= Process.uid.to_s
    def pid_file      = @pid_file      ||= "#{server_area}/pid"
    def server_area   = @server_area   ||= "#{base_path}/server"
    def work_root     = @work_root     ||= "#{Dir.tmpdir}/#{File.basename($0)}"

    ## Find and cache a tmp dir to write to that is probably in memory.
     #
    def fast_tmp_dir
      @fast_tmp_dir ||= begin
        result = %w[/dev/shm /tmp].detect { File.writable?(_1) }
        result or raise "Cannot find writable temp dir"
      end
    end

    ## Find and cache current context's classname in slug form.
     #
    def my_classname
      @my_classname ||= begin
        myclass = (Module === self) ? self : self.class
        myclass.name.gsub(/\W+/, "-")
      end
    end
  end
end
end
