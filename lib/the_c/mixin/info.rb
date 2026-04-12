# frozen_string_literal: true

module TheC
module Mixin
  ## Generic methods for system or context info.
   #
  module Info
    ## Query OS for total number of CPU threads and cache it.
     #
    def cpu_count
      @cpu_count ||= IO.read("/proc/cpuinfo").scan(/^processor/).size
    end

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
