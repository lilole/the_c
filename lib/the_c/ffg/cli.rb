# frozen_string_literal: true

module TheC
module Ffg
  class Cli
    include TheC::Mixin::Say
    include TheC::Mixin::WeOk
    include Usage

    attr_reader :args, :cfg

    def initialize(argv, io: $stderr)
      TheC::Mixin::Say.io = io
      TheC::Mixin::Say.io.sync = true
      @args = argv
      @cfg = Configuration.new
      parse_args
    end

    def run
      Core.new(cfg).run
      true
    rescue Exception => e
      case e
      when Errno::EPIPE then msg = "Broken pipe"
      when Interrupt    then msg = "Caught interrupt"
      when SystemExit   then msg = nil
      else msg = e.full_message
      end
      say msg, "\n" if msg
      false
    end

    def parse_args
      re_opt = { "i" => 1, "x" => 2, "m" => 4 } # Map option chars to Regexp bit values

      force = false; i = -1
      while (arg = args[i += 1])
        if ! force && arg[0] == "-"
          arg == "--" and (force = true; next)
          arg =~ /^-[^-]*[?h]|^--help$/ and usage
          ok!(false)
          arg =~ /^-[^-]*c|^--ctxt(=(.+))?$/     && ok! and cfg.ctxt = ($~[2] || args[i += 1]).to_i
          arg =~ /^-[^-]*D|^--other-devices$/    && ok! and cfg.keep_dev = false
          arg =~ /^-[^-]*d|^--dot-dirs$/         && ok! and cfg.dot_dirs = ! cfg.dot_dirs
          arg =~ /^-[^-]*g|^--fg$/               && ok! and cfg.fg = true
          arg =~ /^-[^-]*[imx]/                  && ok! and arg.each_char { cfg.re_opts ^= (re_opt[_1] || 0) }
          arg =~ /^-[^-]*N|^--no-skips$/         && ok! and cfg.skip_res.clear
          arg =~ /^-[^-]*n|^--skip(=(.+))?$/     && ok! and cfg.skip_res << ($~[2] || args[i += 1])
          arg =~ /^-[^-]*p|^--path-only$/        && ok! and cfg.path_only = true
          arg =~ /^-[^-]*q|^--quiet$/            && ok! and cfg.quiet = true
          arg =~ /^-[^-]*s|^--source-tool-dirs$/ && ok! and cfg.src_dirs = true
          usage "Invalid option: #{arg.inspect}" if ! ok?
        else
          cfg.path_args << arg.sub(%r`/+$`, "")
        end
      end

      usage "Param `regex` is required." if cfg.path_args.size < 1
      cfg.path_args.unshift(".")         if cfg.path_args.size < 2

      if ! cfg.fg
        cfg.re_opts ^= re_opt["i"]    # Find base is ignore case
        cfg.dot_dirs = ! cfg.dot_dirs # Find base is to descend dot dirs
      end

      cfg.skip_res.map! { |re| Regexp.new(re, cfg.re_opts) }
      cfg.re_arg = Regexp.new(cfg.path_args.delete_at(-1), cfg.re_opts)
    end
  end
end
end
