# frozen_string_literal: true

require "shellwords"

module TheC
  class SetPath
    def usage(msg=nil, exit_code=1)
      $stderr << <<~END

      #{msg || "Online help."}

      Description:
        Emit bash code to modify a given env var that contains paths, e.g. PATH. The new value is clean of any
        duplicate or nonexistent dirs, with all dirs normalized.

      Usage:
        setpath [-fqv] VAR [DIR ... [POS]]

      Where:
        -f => Force adding DIR even if it doesn't exist.
        -q => Don't output the final value of the VAR.
        -v => Output the final value of the VAR.
        VAR => Variable name to modify, e.g. "PATH".
        DIR => New path to set in VAR.
        POS => Position in VAR to place DIR; default 1; 0 removes; < 0 counts from end; may use "head" or "tail".

      END
      exit(exit_code) if exit_code
    end

    def run(args)
      force = quiet = var_name = pos = nil
      pos_re = /^[-+]?\d+$/; new_dirs = []
      i = -1
      while (arg = args[i += 1])
        if arg[0] == '-' && arg !~ pos_re
          ok = 0
          arg =~ /^-[^-]*f/ && ok = 1 and force = true
          arg =~ /^-[^-]*q/ && ok = 1 and quiet = true
          arg =~ /^-[^-]*v/ && ok = 1 and quiet = false
          ok > 0 or usage "Invalid arg: #{arg.inspect}"
        elsif ! var_name then var_name = arg
        else  new_dirs << arg
        end
      end

      usage "VAR is required." if ! var_name
      pos = new_dirs.last =~ pos_re && new_dirs.pop
      if    ! pos || pos == "head" then pos = 1
      elsif pos == "tail" then pos = -1
      else  pos = pos.to_i
      end
      quiet.nil? and quiet = new_dirs.any?

      cur_path = ENV[var_name]
      if new_dirs.any?
        to_dir = ->(d) { d = File.realpath(d) rescue d; force || File.directory?(d) ? d : nil }
        val_dirs = new_dirs.map(&to_dir).compact
        dirs = cur_path.split(":").map(&to_dir).compact - new_dirs
        pos > 0 and dirs[pos - 1, 0] = val_dirs
        pos < 0 and dirs[pos, 1] = [dirs[pos]].concat(val_dirs)
        cur_path = dirs.uniq.join(":")
      end
      ENV[var_name] = cur_path
      cmd = "#{var_name}=#{cur_path.shellescape}"
      quiet or $stderr << cmd << "\n"
      new_dirs.any? ? cmd : "true"
    end
  end
end
