# frozen_string_literal: true

require "shellwords"
require "stringio"

module TheC
module Shortcuts
  ## Helper methods for use by defined shortcuts, which may rely on context in
   # their includer class.
   #
  module Helpers
    include TheC::Mixin::Util

    ## Instance access to the names added with the `add` DSL method.
     #
    def added_names = self.class.added_names

    ## The internal Rubyized `c` function, which calls shortcuts previously
     # defined with `add` from within Ruby code. Return values are forced to
     # follow the rules defined in the main comments "Notes:" section.
     # If the given `name` is not defined, then assume the `:zzz` shortcut.
     #
    def c(*args)
      args = process_encodes(args)
      return list if args.empty? || args[0] =~ /^-[^-]*[h?]|^--help$/
      name = args.shift.to_sym
      raise "c: Got nop command" if name == :":"
      result = added_names.key?(name) ? send(name, *args) : zzz(name, *args)
      safe_result(result)
    rescue Exception => e
      quiet = SystemExit === e || e.message.empty?
      puterr "#{e.class}: #{e.message}" if ! quiet
      status = SystemExit === e && e.status # Boolean or Integer
      safe_result(status)
    end

    ## Shorthand to combine calling `bash()` with result of `c()`.
     # The end of the args may be `:echo` or `:errs` to match the same options
     # supported by `bash()`.
     #
    def cc(*args, **bash_opts2)
      bash_opts = []
      while %i[echo errs].member?(args.last)
        bash_opts << args.pop
      end
      bash(c(*args), *bash_opts, **bash_opts2)
    end

    ## Extract and eval special args sent from the `c` Bash function, formatted
     # like `{{ <ruby_code> }}`. This is needed for things like the current dir
     # and exitcode of the last command that ran.
     #
    def process_encodes(args)
      encodes = []
      args.map do |arg|
        if arg =~ /^\s*\{\{(.+?)\}\}\s*$/
          encodes << $~[1]
          nil
        else
          arg
        end
      end.compact.tap do
        encodes.each { eval(_1) }
      end
    end

    ## Ensure the given result is safe for `eval` to run it in bash. This logic
     # follows the rules defined in the main comments "Notes:" section.
     #
    def safe_result(result)
      if    String === result  then result.chomp
      elsif Integer === result then "_() { return #{result}; }; _"
      elsif result.nil?        then ":"
      else  (!! result).to_s
      end
    end

    ## Pass an IO to a user code block which captures lines written to it in a
     # temp file. Then return bash code that uses the `:m` shortcut, which should
     # run a pager command (e.g. `less`), to view that file.
     #
    def page(*args)
      tmpfile = make_temp_file

      # Let the user code block run without waiting
      run = Thread.new do
        begin
          yield(tmpfile.io)
        rescue Exception => e
          quiet = SystemExit === e
          tmpfile.io << e.full_message if ! quiet
        end
      end

      ## Things are better if the entire file to view is written. So here we
       # pause up to 3 secs for the client code to finish.
      run_done = -> { ! run.status || run.status == "aborting" }
      ts_end = Time.now + 3
      sleep 0.1 while ! run_done[] && Time.now < ts_end

      ## Now ready to pass control back to bash and display the file.
       # We use `tail` here if the client code is still running, to force the
       # pager to not assume the file is completely written.
      pager = c(:m, *args)
      if run_done[]
        "#{pager} -F #{tmpfile.path.shellescape}" # `-F` = exit if 1 screen
      else
        # To keep the file open use `.io.path` here, not `.path`
        "tail -f -n 7K #{tmpfile.io.path.shellescape} | #{pager} -+F" # `-+F` = stay open
      end
    end

    ## Pass an IO to a user code block which captures lines written to it in
     # a String, and return that String as the result.
     #
    def strout = StringIO.open { |io| yield(io); io.rewind; io.read }

    ## List all defined shortcuts sorted with descriptions.
     #
    def list
      page do |io|
        width = added_names.keys.map(&:size).max
        added_names.sort.each.with_index do |(name, tup), i|
          n = (i + 1).to_s.rjust(2)
          name = "#{name} ".ljust(width + 4, ".")
          io << "#{n}. #{name} #{tup[0]}\n"
        end
      end
    end

    ## Reusable code that calls shortcuts to find the parent of the first `.git`
     # dir above each given dir, or all `.git` dir parents below each given dir.
     #
    def find_git_workspaces(dirs)
      dirs.map do |dir|
        root = cc(:proot, ".git", dir).line # Search up tree
        if root
          roots = [root]
        else
          roots = cc(:roots, ".git", dir).lines # Search down tree
          raise "Cannot find any git workspaces: #{dir.inspect}." if roots.empty?
        end
        roots
      end.flatten
    end

    ## Run the given lines of code in the foreground, and write the code's result
     # as a single line of bash code to evaluate in the caller's context.
     # This is required for any shortcut that needs to read stdin, because this
     # command service runs in the background.
     # The given code must NOT touch stdout, only stdin and stderr.
     #
    def foreground_run(code, argv_varname=nil, argv=nil)
      if argv_varname
        raise "Both `argv_varname` and `argv` are required" if ! argv
      else
        argv_varname = "#"
        argv = []
      end

      ## We want to make sure the given code truly runs in the same context as any
       # other defined shortcut, with exception handling similar to method `c`.
      code = <<~END
        load #{$0.inspect}

        shortcuts = TheC::Shortcuts::Core.new

        def shortcuts.__foreground_run(*__args)
          #{argv_varname} = __args
          #{code}
        rescue Exception => e
          quiet = SystemExit === e || e.message.empty?
          $stderr << e.full_message if ! quiet
          SystemExit === e && e.status # Boolean or Integer
        end

        result = shortcuts.__foreground_run(*#{argv.inspect})
        $stdout << shortcuts.safe_result(result)
      END

      temp = make_temp_file(code)
      "$(ruby #{temp.path.shellescape})"
    end

    ## Map certain keywords to ANSI color escape sequences.
     #
    def color_escape(words)
      @color_escape ||= begin
        {
          fblack:   30, fred:       31, fgreen:   32, fyellow:   33,
          fblue:    34, fmagenta:   35, fcyan:    36, fwhite:    37,
          fbblack:  90, fbred:      91, fbgreen:  92, fbyellow:  93,
          fbblue:   94, fbmagenta:  95, fbcyan:   96, fbwhite:   97,
          bblack:   40, bred:       41, bgreen:   42, byellow:   43,
          bblue:    44, bmagenta:   45, bcyan:    46, bwhite:    47,
          bbblack: 100, bbred:     101, bbgreen: 102, bbyellow: 103,
          bbblue:  104, bbmagenta: 105, bbcyan:  106, bbwhite:  107,
          blink:     5, nblink:     25, reset:     0
        }.map { |k, v| [k.to_s, v.to_s] }.to_h
      end

      words.map do |word|
        word = word.to_s
        (code = @color_escape[word]) ? "\e[#{code}m" : word
      end.join
    end

    ## Colorize all matches of a regex.
     #
    def color_matches(text, regex, cole_name)
      cole = color_escape([cole_name])
      regex = /^/ if regex.source == "."
      text.gsub(regex, "#{cole}\\0#{color_escape(["reset"])}")
    end

    ## Single SOT for our standard options for `less`.
     #
    def less_opts = @less_opts ||= "-FIJMRSWX#8 --status-col-width=1"

    ## State information for the PS1-related shortcuts.
     #
    class Ps1Helper
      attr_accessor :last_rc, :ps1, :x_git
      attr_reader   :context, :last_user

      def initialize(context)
        @context = context
      end

      def check_last_user
        return if last_user == ENV["USER"]
        @last_user = ENV["USER"]
        # Invalidate all attrs that relate to current user
        @ps1 = nil
      end

      def uname_wrap
        # This will be cached in the `ps1` shortcut
        if ENV["USER"] == "root"
          context.color_escape(%w[fbred bblack blink \\u nblink])
        else
          context.color_escape(%w[fbgreen \\u])
        end
      end
    end # Ps1Helper

    ## Single instance of PS1 state info.
     #
    def ps1_helper = @ps1_helper ||= Ps1Helper.new(self)
  end
end
end
