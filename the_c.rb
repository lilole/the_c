#!/usr/bin/env ruby
# frozen_string_literal: true
#
# Copyright 2024 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

### Every shell shortcut macro/script/tool in a single block of Ruby code.
  #
  # Notes:
  # - Requires Ruby 3.1+ and Linux.
  # - This is meant to be pasted into a bashrc to work alongside a custom "c"
  #   function. For an example see:
  #     https://github.com/lilole/the_c/blob/main/bashrc.example
  # - Defined Ruby shortcuts MUST return one of these standard types:
  #   - `nil`.......Reports a nop to the caller.
  #   - `String`....A bit of bash code to evaluate in the caller's context.
  #   - `Integer`...Return the value as an exit code to the caller.
  #   - truthy......Reports a success to the caller.
  #   - falsey......Reports a failure to the caller.
  # - This is designed to be a local command service, by starting up in the
  #   background and communicating with clients by named pipes. This design is
  #   crazy fast but also puts some constraints on the code for I/O.
  # - Shortcut worker methods MUST accept an `io` param if they need to output
  #   results to the tty. The `io` param may simply turn out to be `$stdout`,
  #   but the worker method MUST NOT assume it writes to `$stdout`.
  # - In general shortcuts should only use `$stderr` for info messages to user.
  # - In general shortcuts should try first to return a String that would be
  #   evaluated as bash code in the caller's context. This is the best way to
  #   ensure that shortcuts can call other shortcuts, and can even be used in
  #   pipelines of shortcuts.
  # - It's safe for shortcuts to raise error for any abrupt/abnormal end
  #   condition. The main handler code here will catch all exceptions,
  #   including `SystemExit`, and display the `Error` object's message.
  # - The `:m` shortcut is a special case, because the `page` helper method
  #   uses it internally for cases where large amounts of data may need to be
  #   viewed.
  # - Follow the patterns here to tweak for your own env. All available
  #   features and their usage should become self evident from the patterns.

require "io/console"
require "shellwords"
require "stringio"

module Helpers
  ### The internal Rubyized `c` function, which calls shortcuts previously
    # defined with `add` from within Ruby code. Return values are forced to
    # follow the rules defined in the main comments "Notes:" section.
    # If the given `name` is not defined, then assume the `:zzz` shortcut.
    #
  def c(*args)
    if args.empty? || args.any? { _1 =~ /^-[^-]*[h?]|^--help$/ }
      list
      return "false"
    end

    name = args.shift
    sc = @index[name.to_sym]
    result = sc ? sc[0].call(*args) : @index[:base][0].call(name, *args)

    if    String === result  then result
    elsif Integer === result then "_() { return #{result}; }; _"
    elsif result.nil?        then ":"
    else  (!! result).to_s
    end
  rescue Exception => e
    quiet = SystemExit === e && e.status == 0 || e.message.empty?
    puterr "#{e.class}: #{e.message}" if ! quiet
    "false"
  end

  # Shorthand for outputting error/warning/info messages to user's console.
  #
  def puterr(*strings)
    strings << "\n" if strings.empty? || ! strings.delete(:nolf)
    $stderr.write(strings.join)
  end

  ### Add a new Ruby shortcut in a DSL style. See calls to this in `Shortcuts`.
    #
  def add(name, description, body)
    raise "Arg 'body' must be a proc" if ! Proc === body
    @index ||= {}
    @index[name.to_sym] = [body, description.to_s]
  end

  ### Prompt user for confirmation before continuing.
    # The `opts` may be any string of typable characters, with a single
    # uppercase to be the default if user presses Enter.
    # If the choice is "q", then exit immediately.
    # If the choice is "y" or "n", then return Boolean.
    # Any other choice returns the character.
    #
  def ask_continue(prompt="Continue?", opts="Ynq")
    def_reply = opts.gsub(/[a-z]+/, "")
    raise "Only 1 uppercase is allowed: #{opts.inspect}" if def_reply.size > 1
    puterr
    begin
      puterr "#{prompt} [#{opts}] ", :nolf
      reply = $stdin.getch(intr: true).chomp
      reply = def_reply if reply.empty? && ! def_reply.empty?
      lreply = reply.downcase
      puterr lreply
    end until lreply =~ /^[#{opts.downcase}]$/
    puterr
    exit if lreply == "q"
    %w[y n].member?(lreply) ? lreply == "y" : lreply
  end

  ### Call the `:m` shortcut, which should run a pager command (e.g. `less`),
    # with an IO object passed to a client code block for its written lines to
    # be paged.
    #
  def page(*args) = IO.popen(c(:m, *args), "w") { |io| yield(io) }

  ### Pass an IO to a client code block which captures lines written to it in
    # a String, and return that String as the result.
    #
  def strout = StringIO.open { |io| yield(io); io.rewind; io.read }

  ### List all defined shortcuts sorted with descriptions.
    #
  def list
    page do |io|
      width = @index.keys.map(&:size).max
      @index.sort.each.with_index do |(name, tup), i|
        n = (i + 1).to_s.rjust(2)
        name = "#{name} ".ljust(width + 4, ".")
        io << "#{n}. #{name} #{tup[1]}\n"
      end
    end
  end

  ### Run `bash` with input as a script, and return an object with useful
    # details about the completed process. This is a souped-up version of the
    # `%x{}` operator. The return object has these attrs:
    #   exitcode => Integer exit code of the process.
    #   fail? => Boolean true iff the process exited abnormally.
    #   line => The last element of `lines`, maybe nil.
    #   lines => The captured stdout lines as an array of chomped strings.
    #   ok? => Boolean true iff the process exited normally.
    #   okout => If the process exited normally, this will be the `out` attr
    #       value, but if not this will be `nil`.
    #   out => Combined stdout/stderr output captured in a chomped String.
    #   stderr => Only stderr output captured in a chomped String.
    #   stdout => Only stdout output captured in a chomped String.
    # Valid values for `opts` are:
    #   :echo => Also send all process stdout/stderr output to current $stdout.
    #   :errs => Also send all process stderr to current $stderr.
    # Valid keys for `opts2` are:
    #   :echo => If truthy, also send all process stdout/stderr output to given
    #       value, if an IO, or to current $stdout.
    #   :errs => If truthy, also send all process stderr output to given
    #       value, if an IO, or to current $stderr.
    #
  def bash(script, *opts, **opts2)
    bad_opts = opts + opts2.keys - %i[echo errs]
    raise "Invalid opts: #{bad_opts}" if bad_opts.any?
    echo_to = opts2[:echo].then { |opt| (IO === opt) ? opt : (opt && $stdout) }
    errs_to = opts2[:errs].then { |opt| (IO === opt) ? opt : (opt && $stderr) }
    opts.each do |opt|
      opt == :echo and echo_to = $stdout
      opt == :errs and errs_to = $stderr
    end

    pipr_s, pipw_s = IO.pipe # Process's stdout redirect
    pipr_e, pipw_e = IO.pipe # Process's stderr redirect
    begin
      io_s, io_e, io_a = Array.new(3) { StringIO.new } # Capture stdout, stderr, stdout+stderr
      buff_s, buff_e = "".b, "".b # Buffers for stdout, stderr
      myclass = (Module === self) ? self : self.class
      mycname = myclass.name.gsub(/\W+/, "-")

      start  = -> { spawn(["bash", "#{mycname}-bash"], "-c", script, out: pipw_s, err: pipw_e) }
      finish = ->(_) { pipw_s.close; pipw_e.close }
      process  = Thread.new { Process::Status.wait(start.()).tap(&finish) }
      reader_s = Thread.new { [io_a, io_s].each { _1.write(buff_s) } while pipr_s.read(10, buff_s) }
      reader_e = Thread.new { [io_a, io_e].each { _1.write(buff_e) } while pipr_e.read(10, buff_e) }

      stat = process.join.value
      [reader_s, reader_e].each { _1.join }
    ensure
      [pipr_s, pipw_s, pipr_e, pipw_e].each { _1.close }
    end

    if ! defined?(@@bash_result)
      @@bash_result = Struct.new(:exitcode, :fail?, :line, :lines, :ok?, :okout, :out, :stderr, :stdout)
    end
    @@bash_result.new.tap do |result|
      ok = stat.success?
      out_a = (io_a.rewind; io_a.read.chomp)
      out_s = (io_s.rewind; io_s.read.chomp)
      out_e = (io_e.rewind; io_e.read.chomp)
      echo_to.puts(out_a) if echo_to
      errs_to.puts(out_a) if errs_to && ! ok

      result.send("fail?=", ! ok)
      result.send("ok?=",   ok)
      result.exitcode = stat.exitstatus
      result.lines    = out_s.split("\n")
      result.line     = result.lines.last # Maybe nil
      result.okout    = ok ? out_a : nil
      result.out      = out_a
      result.stderr   = out_e
      result.stdout   = out_s
    end
  end

  ### Reusable code that calls shortcuts to find the parent of the first `.git`
    # dir above each given dir, or all `.git` dir parents below each given dir.
    #
  def find_git_workspaces(dirs)
    dirs.map do |dir|
      root = bash(c(:proot, ".git", dir)).line # Search up tree
      if root.nil?
        roots = bash(c(:roots, ".git", dir)).lines # Search down tree
        raise "Cannot find any git workspaces: #{dir.inspect}." if roots.empty?
      else
        roots = [root]
      end
      roots
    end.flatten
  end

  ### Convert a number to a String with thousands separators.
    #
  def commafy(n) = n.to_s.reverse.gsub(/(\d{3})(?=\d)(?!\d*\.)/, "\\1,").reverse

  ### Find a tmp dir to write to that is probably in memory.
    #
  def fast_tmp_dir = %w[/dev/shm /tmp].detect { File.writable?(_1) }
end # Helpers

module Shortcuts
  extend Helpers

  add :at_home, "Return success if the current machine is Dan's home one", ->(*args) do
    "[[ $HOSTNAME == danbook.danamis.com ]]"
  end

  add :at_work, "Return success if the current machine is Dan's work one", ->(*args) do
    "[[ $HOSTNAME == zzzdhiggins-vm ]]"
  end

  add :be, "Shorthand 'bundle exec'", ->(*args) do
    "bundle exec #{args.shelljoin}"
  end

  add :cole, "Mix given strings with xterm* color seqs", ->(*args) do
    map = (@cole_memo ||= {
      fblack:   30, fred:       31, fgreen:   32, fyellow:   33,
      fblue:    34, fmagenta:   35, fcyan:    36, fwhite:    37,
      fbblack:  90, fbred:      91, fbgreen:  92, fbyellow:  93,
      fbblue:   94, fbmagenta:  95, fbcyan:   96, fbwhite:   97,
      bblack:   40, bred:       41, bgreen:   42, byellow:   43,
      bblue:    44, bmagenta:   45, bcyan:    46, bwhite:    47,
      bbblack: 100, bbred:     101, bbgreen: 102, bbyellow: 103,
      bbblue:  104, bbmagenta: 105, bbcyan:  106, bbwhite:  107,
      blink:     5, nblink:     25, reset:     0
    })
    raw = false
    string = args.map do |arg|
      if    arg == :raw           then raw = true; ""
      elsif (v = map[arg.to_sym]) then "\e[#{v}m"
      else  arg
      end
    end.join
    raw ? string : "echo #{string.shellescape}"
  end

  add :d, "Change dirs with various abbreviations", ->(*args) do
    map = {
      "c"   => "#{ENV["HOME"]}/code",
      "b"   => "#{ENV["HOME"]}/code/bash",
      "gh"  => "#{ENV["HOME"]}/code/gh",
      "ghn" => "#{ENV["HOME"]}/code/gh/nuttall",
      "ghs" => "#{ENV["HOME"]}/code/gh/sudoku-rb",
      "r"   => "#{ENV["HOME"]}/code/rb",
      "rn"  => "#{ENV["HOME"]}/code/rb/nuttall",
      "rs"  => "#{ENV["HOME"]}/code/rb/sudoku",
      "s"   => "#{ENV["HOME"]}/shop",
      "t"   => "#{ENV["HOME"]}/tmp",
      "l"   => "/var/log",
      "uNN" => "(cd up NN dir levels)"
    }
    usage = -> do
      width = map.values.map(&:size).max
      puterr "Valid abbrevs:"
      map.sort { |a, b| a[1] <=> b[1] }.each do |abbrev, dir|
        dir = (dir + " ").ljust(width + 4, ".")
        puterr "  #{dir} #{abbrev}"
      end
      return false
    end
    dir = (args[0] =~ /^u(\d+)$/) ? ([".."] * $~[1].to_i).join("/") : map[args[0]]
    usage.() if ! dir
    subs = args[1..-1].join("/")
    subs.empty? or dir = Dir.glob("#{dir}/#{subs}").first || "#{dir}/#{subs}"
    "cd #{dir.shellescape} && pwd"
  end

  add :dc, "File diff with colors and paging", ->(*args) do
    page { |io| bash("diff -U5 --color=always #{args.shelljoin}", echo: io).ok? }
  end

  add :dns, "Test homer DNS server", ->(*args) do
    return false if bash(c(:at_home)).fail?
    name = args[0] || "goo.gl"
    hr = "_" * 80
    page do |io|
      for rec in %w[A AAAA]
        for ipv, server in [%w[4 192.168.0.2], %w[6 2002:d1f0:3363:10:dea6:32ff:fe18:36db]]
          for flag in %w[notcp tcp]
            args = "#{name} #{rec} +#{flag} -#{ipv} @#{server}"
            io.puts("\n#{hr}\n+ dig #{args}")
            bash("dig #{args}", echo: io).ok? or return false
          end
        end
      end
    end
    true
  end

  add :ds, "Create a new Docker container as a bg service", ->(*args) do
    print "\nName:  "; name  = $stdin.gets.chomp
    print "\nImage: "; image = $stdin.gets.chomp
    ports = []
    loop do
      print "\nPort maps (HOST:CONTAINER[/udp], - when done): "; v = $stdin.gets.chomp
      break if v == "-"
      ports << v
    end
    envs = []
    loop do
      print "\nEnv var (NAME='VALUE', - when done): "; v = $stdin.gets.chomp
      break if v == "-"
      envs << v
    end
    args = %W[
      docker run --log-driver local --log-opt max-size=2m --log-opt max-file=2
        --detach --restart always --tty --name #{name}
    ]
    envs.each  { |v| args += %W[--env #{v}] }
    ports.each { |v| args += %W[--publish #{v}] }
    args << image
    puts "\nRun: #{args.shelljoin}"
    ask_continue("OK?", "yn") ? args.shelljoin : true
  end

  add :e, "Start the preferred editor if given an arg, otherwise just print the editor command", ->(*args) do
    editors = [
      # In order of preference
      { exe: "code-oss", args_if_params: "--reuse-window", skip_if_no_params: true },
      { exe: "vim" },
      { exe: "vi" }
    ]
    found = editors.detect do |tup|
      if    args.none? && tup[:skip_if_no_params] then false
      elsif bash(c(:x, tup[:exe])).ok?            then true
      else  false
      end
    end
    if ! found
      false
    elsif args.any?
      [found[:exe], found[:args_if_params], args.shelljoin].compact.join(" ")
    else
      words = [found[:exe], found[:args_if_no_params]].compact.shelljoin
      "echo #{words}"
    end
  end

  add :ff, "Find paths matching regex", ->(*args) do
    !! page { |io| Ffg::Cli.new(args, io: io).run }
  end

  add :fg, "Find regex in files", ->(*args) do
    !! page { |io| Ffg::Cli.new(%w[--fg] + args, io: io).run }
  end

  add :gb, "Print current branch, or match first regex branch", ->(*args) do
    re = nil; verbose = false
    args.each do |arg|
      if arg[0] == "-"
        ok = 0
        arg =~ /^-[^-]*v/ && ok = 1 and verbose = true
        raise "Invalid option: #{arg.inspect}: Usage: [-v] [regex]" if ok < 1
      else
        if   ! re then re = arg
        else raise "Invalid arg: #{arg.inspect}: Usage: [-v] [regex]"
        end
      end
    end
    brs = bash("git branch").lines.grep(re ? Regexp.new(re) : /./)
    re or brs = brs.grep(/^\*/) # Current branch
    brs.map! { |ln| ln[2..-1] }
    br = brs[0]
    if brs.size == 1 && br != "(no branch)" # Success iff exactly 1 match
      "echo #{br.shellescape}"
    else
      verbose and puterr "Branch regex '#{re}' did not match exactly once: #{brs.inspect}"
      false
    end
  end

  add :gcb, "Checkout first branch whose name matches given regex", ->(*args) do
    (re = args[0]) or raise "Regex is required."
    if (branch = bash(c(:gb, "-v", re), :errs).line)
      "git checkout #{branch.shellescape}"
    else
      false
    end
  end

  add :gd, "Smarter git diff, includes stats for each file", ->(*args) do
    opts = %w[--color=always]; paths = []
    args.each do |arg|
      if    arg[0] == "-"                           then opts << arg
      elsif File.directory?(arg) || File.file?(arg) then paths << arg
      else  puts "Warning: Ignoring arg: #{arg.inspect}"
      end
    end
    paths.empty? and paths << "."
    cmd = ->(path) do
      "git diff #{opts.shelljoin} %PATH% && echo && git diff --stat #{opts.shelljoin} %PATH%" \
        .gsub("%PATH%", path.shellescape)
    end
    ok = true
    page do |io|
      paths.each do |path|
        if File.file?(path)
          io.puts ""
          ok &= bash(cmd.(path), echo: io).ok?
        else # Dir
          lines = bash("git diff --stat #{path.shellescape}").lines
          summary = lines.last
          lines[0..-2].each do |line|
            file = line.split[0]
            if file.empty? || ! File.file?(file)
              io.puts "Warning: Ignoring output word: #{file.inspect}"
              ok = false
            else
              io.puts ""
              ok &= bash(cmd.(file), echo: io).ok?
            end
          end
          io.puts "\nDir: #{path.inspect}: #{summary}"
        end
      end
    end
    ok
  end

  add :gdb, "Delete branch matching regex, with confirmation", ->(*args) do
    local = false; re = nil
    usage = ->(msg) { puts "#{msg}: Usage: gdb [-l|--local] REGEX"; exit(1) }
    args.each do |arg|
      if arg[0] == "-"
        ok = 0
        arg =~ /^-[^-]*l|^--local$/ && ok = 1 and local = true
        usage.("Invalid opt: #{arg.inspect}") if ok < 1
      else
        if   ! re then re = arg
        else usage.("Invalid arg: #{arg.inspect}")
        end
      end
    end
    usage.("Regex is required") if ! re
    br = bash(c(:gb, "-v", re), :errs).line or return false
    cmds = []
    cmds << "git push origin :#{br.shellescape}" if ! local
    cmds << "git branch -D #{br.shellescape}" << "git fetch --prune"
    if ! ask_continue "Run: #{cmds.join(" && ")} ?", "yn"
      puts "Skipped."
      return false
    end
    cmds.each do |cmd|
      puts "\n+ #{cmd}"
      return false if bash(cmd, :echo).fail?
    end
    true
  end

  add :gg, "Git gui", ->(*args) do
    '({ out="$(meld . 2>&1)" || echo "$out"; } &)'
  end

  add :glb, "List branches matching regex, or all by default", ->(*args) do
    re = Regexp.new(args.shift || ".")
    dirs = args.any? ? args : ["."]
    page do |io|
      find_git_workspaces(dirs).each do |dir|
        Dir.chdir(dir) do
          io.puts "\n#{dir}"
          lines = bash("git branch -vv").lines.grep(re)
          io.puts lines.join if lines.any?
        end
      end
    end
    true
  end

  add :gmm, "Refresh and merge regex match of ARGV[0] or master/main down to current branch", ->(*args) do
    this_br = bash(c(:gb), :errs).line
    return false if ! this_br

    other_br = args[0]
    if other_br
      other_br = bash(c(:gb, "-v", other_br), :errs).line
      return false if ! other_br
    else
      other_br = bash(c(:gb, " (master|main)$")).line
      if ! other_br
        puts "Cannot find default branch."
        return false
      end
    end

    puts "Merging #{other_br.inspect} down to #{this_br.inspect}..."
    [
      "git pull",
      "git checkout #{other_br.shellescape}", "git pull",
      "git checkout #{this_br.shellescape}",
      "git merge -X ignore-space-change #{other_br.shellescape}"
    ].each do |cmd|
      puts "\n+ #{cmd}"
      bash(cmd, :echo).ok? or return false
    end
    true
  end

  add :gmu, "Refresh and merge current branch up to regex match of ARGV[0] branch", ->(*args) do
    other_br = args[0]
    if ! other_br
      puts "Upper branch name is required."
      return false
    end

    other_br = bash(c(:gb, "-v", other_br), :errs).line
    return false if ! other_br

    this_br = bash(c(:gb), :errs).line
    return false if ! this_br

    puts "Merging #{this_br.inspect} up to #{other_br.inspect}..."
    [
      "git pull",
      "git checkout #{other_br.shellescape}", "git pull",
      "git merge -X ignore-space-change #{this_br.shellescape}"
    ].each do |cmd|
      puts "\n+ #{cmd}"
      bash(cmd, :echo).ok? or return false
    end
    true
  end

  add :gnb, "Create new branch named ARGV[0] off of regex match of ARGV[1] or current branch", ->(*args) do
    new_br = args[0]
    if ! new_br
      puts "Usage: gnb NEW_BRANCH [BASE]"
      return false
    end

    base = args[1]
    if base
      base = bash(c(:gb, "-v", base), :errs).line
      return false if ! base
    else
      base = bash(c(:gb), :errs).line
      if ! base
        puts "Cannot find default branch."
        return false
      end
    end

    puts "Creating new branch #{new_br.inspect} based on #{base.inspect}..."
    [
      "git pull",
      "git checkout #{base.shellescape}", "git pull",
      "git checkout -b #{new_br.shellescape}",
      "git push -u origin HEAD"
    ].each do |cmd|
      puts "\n+ #{cmd}"
      bash(cmd, :echo).ok? or return false
    end
    true
  end

  add :gs, "Smarter git status, handles git subdirs", ->(*args) do
    # NOTE: You may need to run "git config --global --bool status.relativePaths false" so paths are correct here
    dirs = []; verbose = true
    args.each do |arg|
      if arg[0] == "-"
        ok = 0
        arg =~ /^-[^-]*q|^--quiet$/ && ok = 1 and verbose = false
        raise "Invalid arg: #{arg.inspect}: Usage: gs [--quiet|-q]" if ok < 1
      else
        dirs << arg
      end
    end
    dirs << "." if dirs.empty?

    page do |io|
      cwd_re = Regexp.escape(Dir.pwd)
      files = Hash.new { |h, k| h[k] = [] }
      find_git_workspaces(dirs).each do |dir|
        Dir.chdir(dir) do
          category = commits_ahead = on_branch = nil; files.clear
          ran = bash("git status .")
          raise "git status failed: #{ran.out}" if ran.fail?
          ran.lines.each do |ln|
            if    ln =~ /^\t/   then files[category] << ln
            elsif ln =~ /^.+:$/ then category = ln.gsub(/\s+/, "-")
            elsif ln =~ /^Not currently on any branch/ then on_branch = "NONE"
            elsif ln =~ /^On branch (.+)/              then on_branch = $~[1]
            elsif ln =~ /^Your branch is( ahead of (\S+) by (\d+))/ then commits_ahead = ", #{$~[3]} to push"
            end
          end
          dir = dir.sub(%r{^#{cwd_re}(/|$)}, ".\\1")
          msg = verbose ? " [#{on_branch}#{commits_ahead}]" : ""
          io << "\n#{dir}#{msg}:\n"
          io << files.keys.sort.map { |cat| cat + "\n" << files[cat].join("\n") }.join("\n")
          io << "\n\n"
        end
      end
    end
    true
  end

  add :gu, "Smarter git update, handles git subdirs", ->(*args) do
    dirs = []; other_branch = nil; argi = -1
    while (arg = args[argi += 1])
      if arg[0] == "-"
        ok = 0
        arg =~ /^-[^-]*b|^--other-branch$/ && ok = 1 and other_branch = args[argi += 1]
        raise "Invalid option: #{arg.inspect}: Usage: gu [--other-branch|-b BRANCH] [DIR ...]" if ok < 1
      else
        dirs << arg
      end
    end
    dirs << "." if dirs.empty?

    retcode = 0
    page do |io|
      find_git_workspaces(dirs).each do |dir|
        Dir.chdir(dir) do
          io << "\n#{dir}"

          cur_branch = bash(c(:gb)).line
          if ! cur_branch
            # Switch to master if no current branch is set
            ran = bash("git checkout master || git checkout main")
            if ran.fail?
              io << "\n\n" << ran.out << "\nSkipping #{dir.inspect}.\n"
              retcode |= 0x04
              next
            end
            cur_branch = bash(c(:gb)).line
          end
          io << " [#{cur_branch}]"

          try_other = other_branch && cur_branch != other_branch
          io.puts(try_other ? " => [#{other_branch}]" : "")

          if bash("git pull", echo: io).fail?
            retcode |= 0x08
            next
          end

          if try_other
            ran = bash("git branch -a", errs: io)
            if ran.fail?
              retcode |= 0x02
              next
            end
            if ran.lines.any? { |ln| ln =~ %r`\s(remotes/)?origin/#{other_branch}\b` }
              io.puts "Pulling remote 'origin/#{other_branch}' into current branch '#{cur_branch}'..."
              ran = bash("git pull origin #{other_branch.shellescape}", echo: io)
              if ran.fail?
                retcode |= 0x10
                next
              end
            end
          end
        end
      end
    end
    retcode
  end

  add :h, "Dan's safe history wrapper", ->(*args) do
    if args[0] =~ /^s/
      flags = %w[a]
      msg = "History saved to '$HISTFILE'."
    elsif args[0] =~ /^l/
      flags = %w[a c r]
      msg = "History loaded from '$HISTFILE'."
    else
      puterr "Usage: h {s[ave]|l[oad]}"
      return false
    end
    cmd = flags.map { |f| "history -#{f}" }.join(" && ")
    "#{cmd} && echo #{msg.shellescape}"
  end

  add :hum, "Stop hum noise on the Alienware machine", ->(*args) do
    "echo 0 | #{c :sudo, "tee", "/sys/module/snd_hda_intel/parameters/power_save"}"
  end

  add :jc, "Run journalctl our way", ->(*args) do
    if args.empty?
      args = %w[--no-hostname -e -n7777]
    else
      args = %w[--no-hostname] + args
    end
    args.unshift("env", "SYSTEMD_PAGER=less", "SYSTEMD_LESS=FIJMRSWX --shift 8", "journalctl")
    c(:sudo, *args)
  end

  add :l, "Run ls the preferred way", ->(*args) do
    "ls -alF --block-size=\"'1\" --color=always #{args.shelljoin} 2>&1 | #{c(:m)}"
  end

  add :lc, "Load predefined text into clipboard", ->(*args) do
    clips = {
      "rb0" => <<~END
        # frozen_string_literal: true
        #
        # Copyright 2024 Dan Higgins
        # SPDX-License-Identifier: Apache-2.0

      END
    }

    name = args[0]; clip = clips[name]
    if ! clip
      puts("Usage: CLIP_NAME\nWhere: CLIP_NAME <= #{clips.keys.sort}")
      return false
    end

    "xclip -in -rmlastnl -selection clipboard <<< #{clip.shellescape}" \
      " && echo " + "Loaded clip: #{name.inspect}".shellescape
  end

  add :m, "Run 'more' style viewer the preferred way", ->(*args) do
    "less -FIJMRSWX -#8 -x4 #{args.shelljoin}"
  end

  add :mi, "Probe media info", ->(*args) do
    ok = true
    page do |io|
      script = "ffprobe -loglevel quiet -print_format json -show_format -show_streams -show_chapters "
      for file in args
        io.puts "\n+ #{file.inspect}"
        ok &= bash(script + file.shellescape, echo: io).ok?
      end
    end
    ok
  end

  add :need_screen, "Return success if on tty at home", ->(*args) do
    "#{c :at_home} && #{c :x, "screen"} && [[ ! $WINDIR && $USER == dan && $(tty) = /dev/tty[12] ]]"
  end

  add :need_x, "Return success if startx should be run", ->(*args) do
    "[[ ! $WINDIR && $USER == dan && $(tty) == /dev/tty1 ]]" \
      " && ( #{c :at_home} || #{c :at_work} )" \
      " && ! pgrep '^X(org)?$' &> /dev/null"
  end

  add :o, "Open files in their default viewer apps", ->(*args) do
    if args.empty?
      puterr "No files given."
      return false
    end

    if bash(c(:x, "xdg-open")).fail?
      puterr "Not configured for file viewer in this env."
      return false
    end

    rans = args.map { |f| bash("xdg-open #{f.shellescape}") }
    if rans.any?(&:fail?)
      puterr args.zip(rans.map(&:out)).map { |pair| pair.join("\n") }.join("\n\n")
      return false
    end
    true
  end

  add :pd, "Dan's smart pushd/popd wrapper", ->(*args) do
    s = args[0].to_s; n = s.to_i
    if    s.empty?       then "dirs"
    elsif s =~ /^-\d+$/  then "popd +#{-n - 1}"
    elsif s =~ /^\+\d+$/ then "pushd +#{n - 1}"
    else  "pushd #{s.shellescape}"
    end
  end

  add :proot, "Detect path to parent dir containing named subdir.", ->(*args) do
    name = args[0]
    raise "Name of subdir to search is required." if ! name

    parent = File.realpath(args[1] || Dir.pwd)

    until File.directory?(File.join(parent, name))
      parent == "/" and (parent = ""; break)
      parent = File.dirname(parent)
    end

    "echo #{parent.shellescape}"
  end

  add :ps1, "Generate fancy xterm* PS1 value", ->(*args) do
    s = {}
    %i[fbcyan fbgreen fbmagenta fbred fbwhite fbyellow reset].each do |label|
      s[label.to_s] = c(:cole, :raw, label)
    end
    s["uname"] = c(:ps1_uname_wrap, '\u')

    # Line 1 saves the last command's rc value to show at the end.
    # Line 2 sets window title.
    template = '`c ps1_last_rc_save`'                      \
      '\033]0;\u@\h:\w\007\033]2;\u@\h:\w\007'             \
      '\n{{uname}}{{fbwhite}}@{{fbgreen}}\h{{fbwhite}}:\w' \
      ' {{fbmagenta}}$$ {{fbcyan}}\t'                      \
      '{{fbyellow}}`c ps1_git_details`'                    \
      '{{fbred}}`c ps1_last_rc_show`{{reset}}\n\$ '

    ps1 = template.gsub(/\{\{(\w+)\}\}/, s[$~[1]])

    "echo #{ps1.shellescape}"
  end

  add :ps1_git_details, "Display super abbreviated current git repo info", ->(*args) do
    bash(c(:x, "git")).ok? or return nil
    branch = bash(c(:gb)).line or return nil
    status = bash("git status").lines; bits = +""
    bits << "!" if status.any? { _1.include?("modified:") }
    bits << "x" if status.any? { _1.include?("deleted:") }
    bits << "?" if status.any? { _1.include?("Untracked files") }
    bits << "+" if status.any? { _1.include?("new file:") }
    bits << "*" if status.any? { _1.include?("Your branch is ahead of") }
    bits << ">" if status.any? { _1.include?("renamed:") }
    bits.empty? or bits.insert(0, " ")
    +"echo " << " [#{branch}#{bits}]".shellescape
  end

  add :ps1_last_rc_save, "Save the last command exit code in a shared place for later", ->(*args) do
    if (dir = fast_tmp_dir)
      rc = ENV["THE_C_LAST_RC"]
      if rc
        File.write("#{dir}/ps1_last_rc_#{ENV["USER"]}", rc) rescue nil
      end
    end
    nil
  end

  add :ps1_last_rc_show, "Display the last command exit code, saved before", ->(*args) do
    if (dir = fast_tmp_dir)
      rc = (File.read("#{dir}/ps1_last_rc_#{ENV["USER"]}") rescue nil)
      if rc && rc != "0"
        "echo #{rc.shellescape}"
      end
    end
    nil
  end

  add :ps1_uname_wrap, "Wrap arg in red if root, otherwise green", ->(*args) do
    if ENV["USER"] == "root"
      c :cole, :raw, :fbred, :bblack, :blink, *args, :reset
    else
      c :cole, :raw, :fbgreen, *args
    end
  end

  add :psg, "Find processes", ->(*args) do
    regex = Regexp.new(args[0] || "^", "i")
    lines = bash("ps -ewwH -o sid,pgid,ppid,pid,uid,tty,cmd").lines \
      .map.with_index do |ln, i|
        if i == 0
          c :cole, :raw, :fbgreen, ln, :reset
        elsif regex.match?(ln)
          ln.gsub(regex, "#{c :cole, :raw, :fbyellow}\\0#{c :cole, :raw, :reset}")
        else
          nil
        end
      end.compact
    page("+G") { |io| io.puts lines.join("\n") }
    lines.size > 1
  end

  add :psync, "Sync abs pathnames from current / to matched ones below ROOT/ subdirs", ->(*args) do
    roots = bash(c(:roots, "ROOT")).lines.map { "#{_1}/ROOT" }
    if roots.empty?
      puterr "Abs paths to sync must be created below 'ROOT/' subdirs."
      return false
    end

    page do |io|
      for root in roots
        io.puts "\n#{"_" * 80}\nProcess relative root: #{root.inspect}"

        paths = strout { |sio| Ffg::Cli.new(root, io: sio).run }.split("\n").sort
        paths.each do |path|
          apath = path[root.size .. -1]

          if ! File.exist?(apath)
            io.puts "Warning: Not extant:\t#{apath}"
            next
          end

          if ! File.readable?(apath)
            io.puts "Warning: Not readable:\t#{apath}"
            next
          end

          eapath, epath = [apath, path].map(&:shellescape)
          if bash("cmp -s #{eapath} #{epath}", echo: io).ok?
            io.puts "Unchanged:\t#{apath}"
            next
          end

          io.puts "Syncing:\t#{apath}"
          io.puts '++ TEST: bash("cp #{eapath} #{epath}", echo: io).ok? or return false'
        end
      end
    end
    true
  end

  add :rce, "Edit ~/code/bash/bashrc", ->(*args) do
    c(:e, "#{ENV["HOME"]}/code/bash/bashrc")
  end

  add :rcp, "Propagate .bashrc to servers", ->(*args) do
    if args.any?
      names = args
    else
      names = %w[
        dan@homer:.bashrc pi@homer:.bashrc root@homer:.bashrc
        dan@missybook:.bashrc root@missybook:.bashrc
        dvr@dvr:dan.bashrc root@dvr:.bashrc
      ]
    end

    ok = true
    names.each do |name|
      name.include?(":") or name = "#{name}:hig.bashrc"

      print "#{name} ... "

      if name[0] == "!"
        puts "skipped."
        next
      end

      ok2 = bash("scp -q #{ENV["HOME"].shellescape}/.bashrc #{name.shellescape}", :echo).ok?
      puts "OK." if ok2
      ok &= ok2
    end
    ok
  end

  add :rh, "Recursive sha256 hash", ->(*args) do
    cwd = Dir.pwd
    paths = args.any? ? args : [cwd]

    paths.each do |path|
      if File.directory?(path)
        Dir.chdir(path)
        find = "."
      else
        find = path
      end
      sum = bash("find #{find.shellescape} -type f | sort | xargs -d \\n cat | sha256sum -b", :errs).line
      puts "#{sum[0, 64]} #{File.basename(path)}" if sum
      Dir.chdir(cwd)
    end
    true
  end

  add :rl, "Recursive listing of files or dirs, sortable by date or size or count", ->(*args) do
    !! page { |io| RecursiveList.run(args, io: io) }
  end

  add :roots, "Select roots of dir trees containing a named subdir", ->(*args) do
    !! Roots.run(args)
  end

  add :sce, "Edit ~/.ssh/config", ->(*args) do
    c(:e, "#{ENV["HOME"]}/.ssh/config")
  end

  add :scl, "List ~/.ssh/config defined hosts", ->(*args) do
    page do |io|
      matches = File.read("#{ENV["HOME"]}/.ssh/config").lines.grep(/^\s*Host\s/).sort
      io.puts matches.join
    end
    true
  end

  add :setpath, "Clever path manipulator, guarantees proper ordering and deduping", ->(*args) do
    SetPath.run(args) # Returns bash code to eval
  end

  add :sudo, "Run sudo only if needed", ->(*args) do
    if bash(c(:x, "sudo")).ok? && ENV["USER"] != "root"
      args = %w[sudo] + args
    end
    args.shelljoin
  end

  add :u, "Run system updater", ->(*args) do
    upd = %w[pacman yum apt apt-get].detect { bash(c(:x, _1)).ok? }

    if ! upd
      puterr "No system updater found."
      return false
    elsif upd == "pacman" && args[0] == "c"
      "checkupdates #{args[1..-1].shelljoin}"
    else
      c(:sudo, upd, *args)
    end
  end

  add :ua, "Run pikaur", ->(*args) do
    if args[0] == "c"
      args = %w[-Qua]
    else
      args = args.map { _1 =~ /^-S/ ? "#{_1}a" : _1 }
    end
    "pikaur #{args.shelljoin}"
  end

  add :ui, "Arch package details, --nn for max depth", ->(*args) do
    depth = 99
    page do |io|
      args.each do |arg|
        if arg =~ /^--(\d+)$/
          depth = $~[1]
          next
        end
        script = "#{c(:u, "-Si", arg)}; #{c(:u, "-Qi", arg)}"
        out = bash(script).lines.map(&:rstrip).reject(&:empty?).uniq.join("\n")
        io.puts "\n#{out}"
        bash("pactree -rd#{depth} #{arg.shellescape}", echo: io)
      end
    end
    true
  end

  add :vb, "View binary files", ->(*args) do
    args = ["-"] if args.empty?
    ok = true
    page do |io|
      args.each do |file|
        ok &= bash("od -Ad -tx1z -w40 #{file.shellescape}", echo: io).ok?
      end
    end
    ok
  end

  add :x, "Are all given commands executable", ->(*args) do
    args.all? { |c| bash("type #{c.shellescape}").ok? }
  end

  add :zzz, "Fallback base case if subcommand is unknown", ->(*args) do
    tbl = { idxs: 0...3, dir: %w[.git .svn CVS], cmd: %w[git svn cvs] }
    idx = tbl[:idxs].detect { |i| bash(c(:x, tbl[:cmd][i])).ok? && bash(c(:proot, tbl[:dir][i])).ok? }
    if idx
      bash("#{tbl[:cmd][idx]} #{args.shelljoin}", :echo).ok?
    else
      puterr "Base: Cannot determine if current dir is git, svn, or cvs. Exiting."; false
    end
  end
end # Shortcuts

module Ffg
  VERSION = "24.714"

  module Usage
    def usage(msg=nil, exit_code=1)
      $stderr << <<~END

      #{msg || "Online help."}

      Description:
        Grep for file paths or contents. (v#{Ffg::VERSION})

      Usage:
        ffg [-imx] [--dot-dirs|-d] [--quiet|-q] [--source-tool-dirs|-s] [--other-devices|-D] \\
            [--no-skips|-N] [--skip|-n path_regex] ... [--] [dir ... file ...] regex

        ffg {--fg|-g} [-imx] [--dot-dirs|-d] [--quiet|-q] [--source-tool-dirs|-s] [--other-devices|-D] \\
            [--ctxt|-c ctxt_lines] [--path-only|-p] \\
            [--no-skips|-N] [--skip|-n path_regex] ... [--] [dir ... file ...] regex

      Where:
        -c, --ctxt => Set context displayed, above and below matches, to `ctxt_lines` lines. Default 0.
        -D, --other-devices => Search for paths across other devices mounted below given dirs.
            Default is to stay within the devices of the given dirs.
        -d, --dot-dirs => Toggle descending dir names that start with ".", except source tool dirs; see -s for those.
            Default with -g (grep) is off, default without -g (find) is on.
        -g, --fg => Grep contents of files; default is to grep for paths.
        -i, -m, -x => Toggle regex flags: ignore case, newline is plain char, and allow whitespace/comments.
            Default with -g (grep) is honor case, default without -g (find) is ignore case.
        -N, --no-skips => Clear list of filtered paths; default is any path ending with tmp, log, coverage,
            (spec|rspec|test|tests)/fixtures.
        -n, --skip => Add regex for paths that will be filtered out and ignored.
        -p, --path-only => Only list unique pathnames that contain a match.
        -q, --quiet => Don't show paths that couldn't be read.
        -s, --source-tool-dirs => Descend .git, .svn, and CVS dirs.

      END
      exit(exit_code) if exit_code
    end
  end # Usage

  class Configuration
    attr_accessor :ctxt, :dot_dirs, :fg, :io, :keep_dev, :path_args, :path_only, :quiet, :re_arg, :re_opts,
      :skip_res, :src_dir_names, :src_dirs

    def initialize
      @ctxt = @re_opts = 0
      @dot_dirs = @fg = @path_only = @quiet = @src_dirs = false
      @keep_dev      = true
      @io            = $stdout
      @path_args     = []
      @skip_res      = %w[/(tmp|log|coverage|(r?spec|tests?)/fixtures)$]
      @src_dir_names = %w[.git .svn CVS].to_set
    end
  end # Configuration

  class Cli
    include Usage

    attr_reader :args, :cfg, :files_queue, :files_thread, :lines_queue, :lines_threads

    def initialize(argv, io: $stdout)
      @args = argv
      @cfg = Configuration.new
      cfg.io = io
    end

    def run
      parse_args(argv)
      start_path_finder
      start_file_processor_pool
      display_lines
      true
    rescue Exception => e
      case e
      when Errno::EPIPE then msg = "Broken pipe"
      when Interrupt    then msg = "Caught interrupt"
      when SystemExit   then msg = (e.status == 0) ? nil : "Abnormal exit"
      else msg = e.full_message
      end
      $stderr.puts(msg, "\n") if msg
      false
    end

    def parse_args(args)
      force = false; re_opt = { "i" => 1, "x" => 2, "m" => 4 }; i = -1
      while (arg = args[i += 1])
        if ! force && arg[0] == "-"
          arg == "--" and (force = true; next)
          ok = 0
          arg =~ /^-[^-]*[?h]|^--help$/          && ok += 1 and usage
          arg =~ /^-[^-]*c|^--ctxt(=(.+))?$/     && ok += 1 and cfg.ctxt = ($~[2] || args[i += 1]).to_i
          arg =~ /^-[^-]*D|^--other_devices$/    && ok += 1 and cfg.keep_dev = false
          arg =~ /^-[^-]*d|^--dot-dirs$/         && ok += 1 and cfg.dot_dirs = ! cfg.dot_dirs
          arg =~ /^-[^-]*g|^--fg$/               && ok += 1 and cfg.fg = true
          arg =~ /^-[^-]*[imx]/                  && ok += 1 and arg.each_char { |c| cfg.re_opts ^= (re_opt[c] || 0) }
          arg =~ /^-[^-]*N|^--no-skips$/         && ok += 1 and cfg.skip_res.clear
          arg =~ /^-[^-]*n|^--skip(=(.+))?$/     && ok += 1 and cfg.skip_res << ($~[2] || args[i += 1])
          arg =~ /^-[^-]*p|^--path-only$/        && ok += 1 and cfg.path_only = true
          arg =~ /^-[^-]*q|^--quiet$/            && ok += 1 and cfg.quiet = true
          arg =~ /^-[^-]*s|^--source-tool-dirs$/ && ok += 1 and cfg.src_dirs = true
          ok > 0 or usage "Invalid param: #{arg.inspect}"
        else
          cfg.path_args << arg
        end
      end
      usage "Param `regex` is required." if cfg.path_args.size < 1
      cfg.path_args.unshift(".")         if cfg.path_args.size < 2
      cfg.fg or (cfg.re_opts ^= 1; cfg.dot_dirs = ! cfg.dot_dirs)
      cfg.skip_res.map! { |re| Regexp.new(re, cfg.re_opts) }
      cfg.re_arg = Regexp.new(cfg.path_args.delete_at(-1), cfg.re_opts)
    end

    def sub_files(dir_or_file, only_device=nil, &block)
      work = [dir_or_file] # Depth-first stack, no recursion needed baby
      begin
        work_path = work.shift
        begin
          if File.directory?(work_path)
            next if cfg.fg && File.symlink?(work_path)
            work[0, 0] = filter!(work_path, Dir.entries(work_path), only_device)
          elsif File.file?(work_path)
            block.call(work_path)
          end
        rescue Errno::EACCES
          cfg.quiet or $stderr.puts "Warning: Skipping unreadable path: #{work_path}"
        end
      end until work.empty?
    end

    def filter!(parent_path, names, only_device=nil)
      names.map! do |name|
        next nil if name == "." || name == ".."
        path = File.join(parent_path, name)
        next nil if cfg.skip_res.any? { |skip_re| skip_re.match?(path.b) }
        next nil if ! cfg.src_dirs && cfg.src_dir_names.member?(name)
        is_d = File.directory?(path)
        next nil if is_d && ! cfg.dot_dirs && name.start_with?(".")
        next nil if is_d && only_device && File.stat(path).dev != only_device
        path
      end
      names.tap { _1.compact!; _1.sort! }
    end

    def grep_file(subfile)
      path = subfile.b; result = []; num = 0; ctxt_val = (cfg.ctxt > 0) ? cfg.ctxt : nil
      ctxt_buf = []; last_match_num = -cfg.ctxt; last_result_num = 1 << 63
      File.open(subfile, "rt") do |f|
        while (num += 1; line = f.gets)
          matched = cfg.re_arg.match?(line.b.chomp)
          if cfg.path_only
            next if ! matched
            break (result << path)
          end
          next unless ctxt_val || matched
          line = "%s:%03d: %s" % [path, num, line]
          if ctxt_val
            if matched
              num - 1 - ctxt_val > last_result_num and result << "...\n"
              result.concat(ctxt_buf) << line
              ctxt_buf.clear
              last_match_num = last_result_num = num
            elsif num - last_match_num <= ctxt_val
              result << line
              last_result_num = num
            else
              ctxt_buf.size < ctxt_val or ctxt_buf.shift(1 + ctxt_buf.size - ctxt_val)
              ctxt_buf << line
            end
          else # Matched, no extra context lines
            result << line
          end
        end
      end
      result
    rescue Errno::EACCES
      cfg.quiet or $stderr.puts "Warning: Skipping unreadable file: #{path}"
      []
    end

    def start_path_finder
      @files_queue = Queue.new
      @files_thread = Thread.new do
        seq = 0
        cfg.path_args.each do |path|
          only_device = cfg.keep_dev ? File.stat(path).dev : nil
          sub_files(path, only_device) { |subfile| files_queue.enq([seq += 1, subfile]) }
        end
        files_queue.close
      end
    end

    def start_file_processor_pool
      @lines_queue = Queue.new
      @lines_threads = (0...file_processors).map do
        Thread.new { cfg.fg ? process_files_grep : process_files_find }
      end
      Thread.new do
        lines_threads.each(&:join)
        lines_queue.close
      end
    end

    def file_processors = Practor.cpu_count

    def process_files_find
      empty = []
      while (seq_path = files_queue.deq)
        p = seq_path[1].b
        lines = cfg.re_arg.match?(p) ? ["#{p}\n"] : empty
        lines_queue.enq([seq_path[0], lines])
      end
    end

    def process_files_grep
      my_actor = Practor.new.start do |actor|
        loop do
          path = actor.receive or break
          actor.send(grep_file(path))
        end
      end
      while (seq_path = files_queue.deq)
        my_actor.send(seq_path[1])
        lines = my_actor.receive
        lines_queue.enq([seq_path[0], lines])
      end
      my_actor.finish
    end

    def display_lines
      cfg.io.sync = true; next_seq = 1; prev_seqs = {}
      while (seq_lines = lines_queue.deq)
        prev_seqs[seq_lines[0]] = seq_lines[1]
        while (lines = prev_seqs.delete(next_seq))
          cfg.io.write(lines.join) if ! lines.empty?
          next_seq += 1
        end
      end
    end
  end # Cli

  ### A rudimentary Ractor based on anonymous pipes.
    #
  class Practor
    attr_reader :actor_pid, :forked, :from_main_r, :from_main_w, :to_main_r, :to_main_w

    def self.cpu_count = @cpu_count ||= ENV["CPUS"]&.to_i || IO.read("/proc/cpuinfo").scan(/^processor/).size

    def initialize
      @from_main_r, @from_main_w = IO.pipe("ASCII-8BIT:ASCII-8BIT")
      @to_main_r,   @to_main_w   = IO.pipe("ASCII-8BIT:ASCII-8BIT")
    end

    def start(&block)
      @actor_pid = fork do
        @forked = true
        cap_unused_ends
        block.call(self)
      end
      Process.detach(actor_pid)
      cap_unused_ends
      self
    end

    def cap_unused_ends
      if forked
        from_main_w.close
        to_main_r.close
      else
        from_main_r.close
        to_main_w.close
      end
    end

    def send(object)
      pipe = forked ? to_main_w : from_main_w
      Marshal.dump(object, pipe)
      pipe.flush
      self
    end

    def receive
      pipe = forked ? from_main_r : to_main_r
      Marshal.load(pipe)
    rescue EOFError
      nil
    end

    def finish = from_main_w.close
  end # Practor
end # Ffg

module RecursiveList
  def self.usage(msg=nil, exit_code=1)
    $stderr << <<~END

    #{msg || "Online help."}

    Description:
      Recursive listing of files or dirs, sortable by date or size or count.

    Usage:
      rl [-cdfnrsuv] [-p pathname] [pathname ...]

    Where:
      pathname = Dir or file to process; default is ".".
      -c = Sort by file count; default is sort by date.
      -d = List dirs only, with their contents sizes and counts.
      -f = List files only, no dirs or links.
      -n = Sort by name; default is sort by date.
      -p = Add pathname even if it starts with "-".
      -r = Reverse sort order; default order is desc, except by name is asc.
      -s = Sort by size; default is sort by date.
      -u = Unsorted; default is sort by date.
      -v = Verbose, including type, perms, and owner.

    END
    exit(exit_code) if exit_code
  end

  class Node
    attr_reader :children, :path, :stat

    def load(path)
      @path, @stat, @children = path, File.lstat(path), []
      stat.directory? and Dir.glob("#{path}/*", File::FNM_DOTMATCH).each do |sub_path|
        sub_path[-2,2] != "/." && sub_path[-3,3] != "/.." and children << Node.new.load(sub_path)
      end
      self
    end

    def size = @size ||= stat.directory? ? (children.reduce(0) { |acc, child| acc + child.size }) : stat.size

    def count = @count ||= stat.directory? ? (children.reduce(0) { |acc, child| acc + child.count }) : 1

    def to_a = children.map { |child| child.to_a }.flatten + [self]
  end # Node

  def self.run(args, io: $stdout)
    only_dirs = only_files = by_count = by_name = by_sz = verbose = false
    sort = 1; paths = []
    argi = -1
    while (arg = args[argi += 1])
      ok = 0
      arg =~ /^-[^-]*[h?]/ && ok = 1 and usage
      arg =~ /^-[^-]*c/    && ok = 1 and by_count = true
      arg =~ /^-[^-]*d/    && ok = 1 and only_dirs = true
      arg =~ /^-[^-]*f/    && ok = 1 and only_files = true
      arg =~ /^-[^-]*n/    && ok = 1 and by_name = true
      arg =~ /^-[^-]*r/    && ok = 1 and sort *= -1
      arg =~ /^-[^-]*s/    && ok = 1 and by_sz = true
      arg =~ /^-[^-]*u/    && ok = 1 and sort = 0
      arg =~ /^-[^-]*v/    && ok = 1 and verbose = true
      arg =~ /^-[^-]*p/    && ok = 1 and paths << args[argi += 1]
      arg !~ /^-/          && ok = 1 and paths << arg
      usage("Invalid option: #{arg.inspect}") if ok < 1
    end

    nodes = (paths.empty? ? ["."] : paths).map { |path| Node.new.load(path).to_a }.flatten

    if sort != 0
      if    by_sz    then nodes.sort! { |a, b| sort * (2 * (b.size <=> a.size) + (a.path <=> b.path)) }
      elsif by_count then nodes.sort! { |a, b| sort * (2 * (b.count <=> a.count) + (a.path <=> b.path)) }
      elsif by_name  then nodes.sort! { |a, b| sort * (a.path <=> b.path) }
      else  nodes.sort! { |a, b| sort * (2 * (b.stat.mtime <=> a.stat.mtime) + (a.path <=> b.path)) }
      end
    end

    lines = []
    for node in nodes
      next if only_dirs && ! node.stat.directory?
      next if only_files && ! node.stat.file?
      if verbose
        details = "%-9s %6o %4d %4d " % [:ftype, :mode, :uid, :gid].map { |v| node.stat.send(v) }
      else
        details = ""
      end
      io << "%s %13s %6s %s%s%s\n" % [
        node.stat.mtime.strftime("%Y-%m-%d %H:%M:%S"),
        Shortcuts.commafy(node.size),
        Shortcuts.commafy(node.count),
        details,
        node.path,
        node.stat.directory? ? "/" : ""
      ]
    end
    true
  rescue => e
    $stderr << "#{e.class}: #{e.message}\n" unless e.message.empty? || Errno::EPIPE === e
    false
  end
end # RecursiveList

module Roots
  def self.usage(msg=nil, exit_code=1)
    $stderr << <<~END

    #{msg || "Online help."}

    Description:
      List each dir node that is a parent of some dir, starting from any given set of dirs.

    Usage:
        roots [--full-tree|-f] dir_name [tree_path ...]

    Where:
        -f, --full-tree => Display full tree to stderr for debugging.
        dir_name => Name of subdir to search for, e.g. ".svn", ".git", etc.
        tree_path => Pathname to the top-level of the dir tree to search; default is ".".

    END
    exit(exit_code) if exit_code
  end

  def self.run(args, io: $stdout)
    full = false; subdir = nil; roots = []
    argi = -1
    while (arg = args[argi += 1])
      if arg[0] == "-"
        ok = 0
        arg =~ /^-[^-]*[h?]|^--help$/   && ok = 1 and usage
        arg =~ /^-[^-]*f|^--full-tree$/ && ok = 1 and full = true
        usage "Invalid option: #{arg.inspect}" if ok < 1
      else
        if   ! subdir then subdir = arg
        else roots << arg
        end
      end
    end
    usage "The dir_name arg is required." if ! subdir
    roots.empty? and roots << "."
    ok = true
    roots.each do |root|
      raise "Not a directory: #{root.inspect}" if ! File.directory?(root)
      tree = Node.new(root, prune_name: subdir)
      full and $stderr << "{### Full tree:\n" + tree.to_tree << "###}\n"
      name_roots = tree.filter(subdir); ok &= name_roots.any?
      io << name_roots.map { |node| File.dirname(File.expand_path(node.pathname)) + "\n" }.join
    end
    ok
  end

  class Node
    attr_reader :children, :pathname

    def initialize(pathname, prune_name: nil)
      @pathname = File.expand_path(pathname.sub(%r`/\s*$`, ""))
      @children = []
      return if prune_name == File.basename(pathname)
      Dir.glob("#{pathname}/*", File::FNM_DOTMATCH).each do |path|
        next if path[-3,3] == "/.." || path[-2,2] == "/." || ! File.lstat(path).directory?
        children << Node.new(path, prune_name: prune_name)
      end
    end

    def filter(name, sort: true)
      [].tap do |result|
        result << self if File.basename(pathname) == name
        children.each { |child| result.concat(child.filter(name, sort: false)) }
        result.sort! if sort
      end
    end

    def to_tree(level=0)
      ind = ". " * level
      s = "#{ind}%s/\n" % (level == 0 ? pathname : File.basename(pathname))
      children.empty? or s << children.map { |child| child.to_tree(level + 1) }.join
      s
    end

    def <=>(other) = pathname <=> other.pathname # For `sort`
  end # Node
end # Roots

module SetPath
  def self.usage(msg=nil, exit_code=1)
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

  def self.run(args)
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
    cmd = "#{var_name}=#{cur_path.shellescape}"
    quiet or $stderr << cmd << "\n"
    new_dirs.any? ? cmd : "true"
  end
end # SetPath

### Main Ruby entry points for bashrc code to use. This manages named pipes,
  # listens for input, and responds the output. This is designed to be a
  # background process that runs alongside the bash process. With this design
  # the size of the code does not matter, and memory caching can be leveraged,
  # and responses are as fast as possible.
  #
module TheC
  class << self
    attr_reader :base_path, :in_out_delims, :my_pid, :pipe_path_i, :pipe_path_o, :ppid
  end
  @my_pid = Process.pid
  @in_out_delims = %w[-i -o] # Separate the base filename from pid for each pipe

  ### Initialize the service and enter event loop.
    #
  def self.start
    @base_path = ARGV[0]
    @ppid      = ARGV[1]&.to_i
    adjust_process
    clean_old_pipes
    install_new_pipes

    while (line = line_input)
      # NOTE: Design flaw alert: For all shortcuts to work, this process should
      #       have the tty here. This is why the bash function that sends
      #       messages first has to do an `fg` call.
      #       A future version will remove this flaw.
      result = Shortcuts.c(*line.shellsplit)

      # NOTE: Hack alert: I could not find a way to force this process to give
      #       its tty control back to the parent bash process. At the very least
      #       I had to press Ctrl+Z. In my environment I'm almost always in a
      #       GUI terminal, so this call simulates that physical keypress.
      #       When not in a GUI, pressing Ctrl+Z will be required.
      system(["xdotool"] * 2, "key", "ctrl+z", [:err, :out] => "/dev/null")

      line_output(result)
    end
  end

  def self.line_input = File.open(pipe_path_i, "r") { _1.gets }

  def self.line_output(line) = File.open(pipe_path_o, "a") { _1.puts(line) }

  def self.install_new_pipes
    @pipe_path_i, @pipe_path_o = in_out_delims.map { "#{base_path}#{_1}#{my_pid}" }
    [pipe_path_i, pipe_path_o].each { File.mkfifo(_1, 0o600) }
  end

  def self.clean_old_pipes
    # Try removing old pipes that somehow got left open
    in_out_delims.each do |delim|
      Dir.glob("#{base_path}#{delim}*").each do |check_path|
        next if Process.uid != File.stat(check_path).uid
        check_pid = check_path.split(delim).last.to_i
        clean_it = (check_pid == my_pid)
        if ! clean_it
          check_cmd = `ps -o command= -p #{check_pid}`
          clean_it = ! check_cmd.start_with?(proctitle_base)
        end
        File.delete(check_path) if clean_it
      end
    end
  end

  def self.adjust_process
    #Process.setpgrp # Use this if signals from bash mess up this process
    Process.setproctitle("#{proctitle_base} #{ppid}")
  end

  def self.proctitle_base = "the_c-ruby #{base_path.shellescape}"
end # TheC
