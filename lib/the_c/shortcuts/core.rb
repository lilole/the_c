# frozen_string_literal: true

require "json"
require "shellwords"

module TheC
module Shortcuts
  ## The actual shortcut definitions, which depend on the 1500+ lines above.
   #
  class Core
    include Dsl
    include Helpers

    add :at_home, "Return success if the current machine is Dan's home one", ->(*args) do
      "[[ $HOSTNAME == danbook.danamis.com ]]"
    end

    add :at_work, "Return success if the current machine is Dan's work one", ->(*args) do
      "[[ $HOSTNAME == zzzdhiggins-vm ]]"
    end

    add :be, "Shorthand 'bundle exec'", ->(*args) do
      "bundle exec #{args.shelljoin}"
    end

    add :cole, "Join given strings with ANSI color seqs", ->(*args) do
      if args.empty?
        usage = <<~END
          Usage:
            cole {CODE|STRING} ...
          Where:
            CODE => One of:
                {{LIST}}
            STRING => Not a CODE, any other String value to include.
        END

        keys  = @color_escape.keys
        width = keys.map(&:size).max
        list  = begin
          keys.each_slice(4).map do |codes|
            codes.map do |code|
              tail = code == codes.last ? "" : "," + " " * (width + 1 - code.size)
              "#{code}#{tail}"
            end.join
          end.join(",\n" + " " * 6)
        end

        puterr(usage.sub("{{LIST}}", list), nolf: true)
        exit(1)
      end

      "echo #{color_escape(args).shellescape}"
    end

    add :d, "Change dirs with various abbreviations", ->(*args) do
      map = {
        b:    "~/code/bash",
        c:    "~/code",
        d:    "~/drive/Truck",
        f:    "~/shop/FLR",
        gh:   "~/code/gh",
        r:    "~/code/rb",
        s:    "~/shop",
        t:    "~/tmp",
        v:    "~/media/Video",
        uNN:  "(cd up NN dir levels)"
      }.map { |k, v| [k.to_s, v.to_s.sub(/^~/, ENV["HOME"])] }.to_h

      dir = (args[0] =~ /^u(\d+)$/) ? ([".."] * $~[1].to_i).join("/") : map[args[0]]
      if ! dir
        width = map.values.map(&:size).max
        puterr "Valid abbrevs:"
        map.sort { |a, b| a[1] <=> b[1] }.each do |abbrev, dir|
          puterr "  %s %s" % ["#{dir} ".ljust(width + 4, "."), abbrev]
        end
        puterr "Examples:\n  $ c d r mp4* # cd to matched subdir\n  $ ls -l `c d v` # List without cd"
        exit(1)
      end

      subs = args[1..-1].join("/")
      subs.empty? or dir = Dir.glob("#{dir}/#{subs}").first || "#{dir}/#{subs}"

      "cd #{dir.shellescape} && pwd"
    end

    add :dc, "File diff with colors and paging", ->(*args) do
      page { |io| bash("diff -U5 --color=always #{args.shelljoin}", echo: io) }
    end

    add :dns, "Test homer DNS server", ->(*args) do
      return false if cc(:at_home).fail?

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
    end

    add :ds, "Create a new Docker container as a bg service", ->(*args) do
      foreground_run <<~'END'
        puterr "\nName:  ", nolf: true; name  = gets.chomp
        puterr "\nImage: ", nolf: true; image = gets.chomp
        ports = []
        loop do
          puterr "\nPort maps (HOST:CONTAINER[/udp], - when done): ", nolf: true
          v = gets.chomp
          break if v == "-"
          ports << v
        end
        envs = []
        loop do
          puterr "\nEnv var (NAME='VALUE', - when done): ", nolf: true
          v = gets.chomp
          break if v == "-"
          envs << v
        end
        args = %W[
          docker run --log-driver local --log-opt max-size=2m --log-opt max-file=2
            --detach --restart unless-stopped --tty --name #{name}
        ]
        envs.each  { |v| args << "--env" << v }
        ports.each { |v| args << "--publish" << v }
        args << image
        puterr "\nRun: #{args.shelljoin}"
        ask_continue("OK?", "yn") ? args.shelljoin : true
      END
    end

    add :e, "Start the preferred editor if given an arg, otherwise just print the editor command", ->(*args) do
      editors = [
        # In order of preference
        { exe: "pulsar", skip_if_no_params: true },
        { exe: "vim" },
        { exe: "vi" }
      ]
      found = editors.detect do |tup|
        if    args.none? && tup[:skip_if_no_params] then false
        elsif cc(:x, tup[:exe]).ok?                 then true
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
      page { |io| Ffg::Cli.new(args, io: io).run }
    end

    add :fg, "Find regex in files", ->(*args) do
      page { |io| Ffg::Cli.new(%w[--fg] + args, io: io).run }
    end

    add :gb, "Print current branch, or match first regex branch", ->(*args) do
      re = nil; verbose = false
      usage = ->(msg) { raise "#{msg}: Usage: gb [-v] [regex]" }
      args.each do |arg|
        if    arg =~ /^-[^-]*[h?]|^--help$/ then usage["Online help"]
        elsif arg == "-v"                   then verbose = true
        elsif ! re                          then re = arg
        else  usage["Invalid arg: #{arg.inspect}"]
        end
      end
      brs = bash("git branch").lines.grep(re ? Regexp.new(re) : /^/)
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
      if (branch = cc(:gb, "-v", re, :errs).line)
        "git checkout #{branch.shellescape}"
      else
        false
      end
    end

    add :gd, "Smarter git diff, includes stats for each file", ->(*args) do
      page do |io|
        opts = %w[--color=always]; paths = []; force = false
        args.each do |arg|
          if    arg == "--"                             then force = true; next
          elsif arg[0] == "-" && ! force                then opts << arg
          elsif File.directory?(arg) || File.file?(arg) then paths << arg
          else  io.puts "Warning: Ignoring arg: #{arg.inspect}"
          end
        end
        paths.empty? and paths << "."

        base = "git diff --relative"
        file_diff = ->(file) do
          o = opts.shelljoin; f = file.shellescape
          io.puts ""
          bash("#{base} #{o} #{f} && echo && #{base} --stat=500 #{o} #{f}", echo: io)
        end

        paths.each do |path|
          if File.file?(path)
            file_diff[path]
          else # Dir
            lines = bash("#{base} --stat=500 #{path.shellescape}").lines
            summary = lines.last
            lines[0..-2].each do |line|
              file = line.split(/ +\| /)[0]&.[](1..-1)
              if file && ! file.empty? && File.file?(file)
                file_diff[file]
              else
                io.puts "Warning: Ignoring output word: #{file.inspect}"
              end
            end
            io.puts "\nDir: #{path.inspect}: #{summary}"
          end
        end
      end
    end

    add :gdb, "Delete branch matching regex, with confirmation", ->(*args) do
      code = <<~'END'
        usage = ->(msg) { raise "#{msg}: Usage: gdb [--local|-l] REGEX" }

        local = false; re = nil
        args.each do |arg|
          if    arg =~ /^-[^-]*l|^--local$/ then local = true
          elsif ! re then re = arg
          else  usage["Invalid arg: #{arg.inspect}"]
          end
        end
        usage["Regex is required"] if ! re

        br = cc(:gb, "-v", re, :errs).line or return false
        cmds = []
        cmds << "git push origin :#{br.shellescape}" if ! local
        cmds << "git branch -D #{br.shellescape}"
        cmds << "git fetch --prune"

        if ! ask_continue "Run: #{cmds.join(" && ")} ?", "yn"
          $stderr.puts "Skipped."
          return false
        end

        cmds.each do |cmd|
          $stderr.puts "\n+ #{cmd}"
          ran = bash(cmd)
          $stderr.puts ran.out
          return false if ran.fail?
        end

        true
      END
      foreground_run(code, "args", args)
    end

    add :gg, "Git gui", ->(*args) do
      '({ out="$(meld . 2>&1)" || echo "$out"; } &)'
    end

    add :glb, "List branches matching regex, or all by default", ->(*args) do
      re = Regexp.new(args.shift || "^")
      dirs = args.any? ? args : ["."]
      page do |io|
        find_git_workspaces(dirs).each do |dir|
          Dir.chdir(dir) do
            ran = bash("git branch -vv")
            lines = ran.ok? ? ran.lines.grep(re).join("\n") : ran.out
            lines = "(No matches.)" if lines.empty?
            io.puts "\n#{dir}\n#{lines}"
          end
        end
      end
    end

    add :gmm, "Refresh and merge regex match of ARGV[0] or master/main down to current branch", ->(*args) do
      this_br = cc(:gb, "-v", :errs).line
      return false if ! this_br

      if (other_br = args[0])
        other_br = cc(:gb, "-v", other_br, :errs).line
        return false if ! other_br
      else
        other_br = cc(:gb, "-v", " (master|main)$").line
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
      if ! (other_br = args[0])
        puts "Upper branch name is required."
        return false
      end

      other_br = cc(:gb, "-v", other_br, :errs).line
      return false if ! other_br

      this_br = cc(:gb, "-v", :errs).line
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
      if ! (new_br = args[0])
        puts "Usage: gnb NEW_BRANCH [BASE]"
        return false
      end

      if (base = args[1])
        base = cc(:gb, "-v", base, :errs).line
      else
        base = cc(:gb, "-v", :errs).line
      end
      return false if ! base

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
      usage = ->(msg) { puterr "#{msg}: Usage: gs [--quiet|-q] [--] DIR ..."; false }
      dirs = []; verbose = true; force = false; argi = -1
      args.each do |arg|
        arg == "--" and (force = true; next)
        if arg[0] == "-" && ! force
          c = 0; i, a = arg[0..1] == "--" ? [arg.size, 0] : [1, 1] # TODO: Make this standard boilerplate
          arg =~ /^-[^-]*q|^--quiet$/ && c += i and verbose = false
          return usage["Invalid opt: #{arg.inspect}"] if c < arg.size - a
        else
          dirs << arg
        end
      end
      if dirs.empty?
        raise "Cannot find workspace root" if ! (arg = cc(:proot, ".git").line)
        dirs << arg
      end

      page do |io|
        cwd_re = Regexp.escape(Dir.pwd)
        files = Hash.new { |h, k| h[k] = [] }
        dirs.each do |dir|
          Dir.chdir(dir) do
            category = commits_ahead = on_branch = nil
            files.clear
            ran = bash("git status .")
            raise "git status failed: #{ran.out}" if ran.fail?

            ran.lines.each do |ln|
              if    ln =~ /^.+:$/ then category = ln.gsub(/\s+/, "-")
              elsif ln =~ /^\t/   then files[category] << ln.sub(/^\t+/) { " " * (_1.size * 4) }
              elsif ln =~ /^Not currently on any branch/ then on_branch = "NONE"
              elsif ln =~ /^On branch (.+)/              then on_branch = $~[1]
              elsif ln =~ /^Your branch is ahead of \S+ by (\d+)/ then commits_ahead = ", #{$~[1]} to push"
              end
            end

            dir = dir.sub(%r{^#{cwd_re}(/*|$)}, ".\\1")
            msg = verbose ? " [#{on_branch}#{commits_ahead}]" : ""
            file_lines = files.keys.sort.map { |cat| cat + "\n" << files[cat].join("\n") }.join("\n")

            io << "\n#{dir}#{msg}:\n#{file_lines}\n"
          end
        end
      end
    end

    add :gu, "Smarter git update, handles git subdirs", ->(*args) do
      usage = ->(msg) { puterr "#{msg}: Usage: gu [--other-branch|-b] [[--dir|-d] DIR] ..."; return false }
      dirs = []; other_branch = nil; argi = -1
      while (arg = args[argi += 1])
        arg =~ /^-[^-]*[h?]|^--help$/ and usage["Online help"]
        if arg[0] == "-"
          c = 0; i, a = arg[0..1] == "--" ? [arg.size, 0] : [1, 1] # TODO: Helper method for this
          arg =~ /^-[^-]*b|^--other-branch$/ && c += i and other_branch = args[argi += 1]
          arg =~ /^-[^-]*d|^--dir$/          && c += i and dirs << args[argi += 1]
          usage["Invalid opt: #{arg.inspect}"] if c < arg.size - a # TODO: See TODO 3 lines up
        else
          dirs << arg
        end
      end
      dirs << "." if dirs.empty?

      page do |io|
        find_git_workspaces(dirs).each do |dir|
          Dir.chdir(dir) do
            io << "\n#{dir}"

            cur_branch = cc(:gb).line
            if ! cur_branch
              # Switch to master if no current branch is set
              ran = bash("git checkout master || git checkout main")
              if ran.fail?
                io << "\n\n" << ran.out << "\nSkipping #{dir.inspect}.\n"
                next
              end
              cur_branch = cc(:gb).line
            end
            io << " [#{cur_branch}]"

            try_other = other_branch && cur_branch != other_branch
            io.puts(try_other ? " => [#{other_branch}]" : "")

            next if bash("git pull", echo: io).fail?

            if try_other
              ran = bash("git branch -a", errs: io)
              next if ran.fail?

              if ran.lines.any? { |ln| ln =~ %r`\s(remotes/)?origin/#{other_branch}\b` }
                io.puts "Pulling remote 'origin/#{other_branch}' into current branch '#{cur_branch}'..."
                next if bash("git pull origin #{other_branch.shellescape}", echo: io).fail?
              end
            end
          end
        end
      end
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
      less = "#{less_opts} +G"
      args = %W[env SYSTEMD_PAGER=less SYSTEMD_LESS=#{less} journalctl --no-hostname] + args
      args.member?("--user") ? args.shelljoin : c(:sudo, *args)
    end

    add :l, "Run ls the preferred way", ->(*args) do
      "ls -alF --group-directories-first --block-size=\"'1\" --color=always #{args.shelljoin} 2>&1 | #{c :m}"
    end

    add :lc, "Load predefined text into clipboard", ->(*args) do
      clips = {
        "rb0" => (<<~END),
          # frozen_string_literal: true
          #
          # Copyright 2024 Dan Higgins
          # SPDX-License-Identifier: Apache-2.0

        END
        "rbm" => (<<~END),
          ## X
           #
          module X
            #
          end
        END
        "rbc" => (<<~END),
          ## X
           #
          class X
            #include X

            attr :x

            def initialize(x)
              @x = x
            end
          end
        END
      }

      name = args[0]; clip = clips[name]
      if ! clip
        puterr "Usage: CLIP_NAME\nWhere: CLIP_NAME <= #{clips.keys.sort}"
        return false
      end

      one_line = clip.inspect.gsub("$", "\\\\$") # We need this because `.shellescape` leaves LFs

      "echo -e #{one_line} | xclip -in -rmlastnl -selection clipboard" \
        " && echo " + "Loaded clip: #{name.inspect}".shellescape
    end

    add :less_options, "The standard opts for `less`", ->(*args) do
      "echo #{less_opts.shellescape}"
    end

    add :m, "Run 'more' style viewer the preferred way", ->(*args) do
      "less #{less_opts} #{args.shelljoin}"
    end

    add :mi, "Probe media info", ->(*args) do
      files = []; full = help = nil; idx = -1
      while (arg = args[idx += 1])
        arg =~ /^-[^-]*[h?]|^--help$/ and help = true
        arg =~ /^-[^-]*f|^--full$/    and full = true
        arg[0] != "-" and files << arg
      end
      help ||= files.empty? || ! files.all? { File.readable?(_1) }
      if help
        puterr(<<~END)
          Usage: mi [--full|-f] FILE ...
          Where:
            -f, --full => Show all discovered media params as raw JSON.
                Default is to show common params.
        END
        return false
      end
      subject = {}
      detect = ->(*names) { subject[names.detect { |n| subject[n] }] || "?" }
      to_num = ->(*names) do
        s = detect[*names].strip
        f = (a, b = s[/\A([\d.]+)[^\d.]+([\d.]+)\z/, 1, 2].map(&:to_f); a / b) rescue Float::NAN
        f.nan? ? s.to_f : f
      end
      with_num = ->(*names) { "#{commafy(to_num[*names].round(3))} (#{detect[*names]})" }
      with_kbps = ->(*names) { "#{commafy((to_num[*names] / 1000).round)} Kbps (#{detect[*names]})" }
      with_time = ->(*names) { n = to_num[*names].round; "#{n / 60}m #{n % 60}s (#{detect[*names]})" }
      common = ->(json) do
        hash = JSON.parse(json); text = +""
        for stream in hash["streams"]
          subject.clear.merge!(stream)
          case stream["codec_type"]
          when "video"
            text << <<~END
              Video_stream:
                Index:      #{detect["index"]}
                Codec:      #{detect["codec_long_name", "codec_name"]}
                Size:       #{detect["width"]}x#{detect["height"]} px
                Frame_rate: #{with_num["avg_frame_rate", "r_frame_rate"]} hz
                DAR:        #{with_num["display_aspect_ratio", "dar"]}
                SAR:        #{with_num["sample_aspect_ratio", "sar"]}
                Bitrate:    #{with_kbps["bit_rate"]}
            END
          when "audio"
            text << <<~END
              Audio_stream:
                Index:       #{stream["index"]}
                Codec:       #{detect["codec_long_name", "codec_name"]}
                Sample_rate: #{commafy(detect["sample_rate"])} hz
                Channels:    #{detect["channels"]}
                Bitrate:     #{with_kbps["bit_rate"]}
            END
          when "subtitle"
            text << <<~END
              Subtitle_stream:
                Index:       #{stream["index"]}
                Codec:       #{detect["codec_long_name", "codec_name"]}
                Bitrate:     #{commafy(detect["bit_rate"])} bps
                Frames:      #{detect["nb_frames"]}
            END
          else
            text << <<~END
              Unknown_stream:
                Index: #{stream["index"]}
                Raw:   #{stream.inspect}
            END
          end
        end
        subject.clear.merge!(hash["format"])
        text << <<~END
          Container:
            Name:     #{detect["format_long_name", "format_name"]}
            Size:     #{commafy(detect["size"])} B
            Chapters: #{(hash["chapters"] || []).size}
            Duration: #{with_time["duration"]}
            Bitrate:  #{with_kbps["bit_rate"]}
        END
      end
      script = "ffprobe -hide_banner -loglevel quiet -print_format json" \
        " -show_format -show_streams -show_chapters -analyzeduration #{4 * 3600 * 10**6}"
      page do |io|
        for file in files
          io.puts "\n+ #{file.inspect}"; gb = 10**9
          sz = (v = File.size(file)) > 16*gb ? v / 4 : (v > 8*gb ? v / 2 : (v > 4*gb ? 4*gb : v))
          raw = `#{script} -probesize #{sz} #{file.shellescape}`.strip
          io.puts(full ? raw : common[raw])
        end
      end
    end

    add :need_screen, "Return success if on a tty at home", ->(*args) do
      "#{c :at_home} && #{c :x, "screen"}" \
        " && [[ ! $WINDIR && $USER == dan && $(tty) == /dev/tty[12] ]]"
    end

    add :need_x, "Return success if startx should be run", ->(*args) do
      "(#{c :at_home} || #{c :at_work})" \
        " && [[ ! $WINDIR && $USER == dan && $(tty) == /dev/tty1 ]]" \
        " && ! pgrep '^X(org)?$' &> /dev/null"
    end

    add :o, "Open files in their default viewer apps", ->(*args) do
      if args.empty?
        puterr "No files given."
        return false
      end

      if cc(:x, "xdg-open").fail?
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

    add :ps1, "Generate fancy xterm PS1 value", ->(*args) do
      ps1_helper.check_last_user
      ps1_helper.ps1 ||= begin
        # Set the full PS1 contents here.
        # Line 1 saves the last command's rc value to show at the end.
        # Line 2 sets window title.
        template = begin
          '`c ps1_last_rc_save`'                        \
          '\033]0;\u@\h:\w\007\033]2;\u@\h:\w\007'      \
          '\n{{uname}}{{fbwhite}}@{{fbgreen}}\h'        \
          '{{fbwhite}} \w{{fbmagenta}} $${{fbcyan}} \t' \
          '{{fbyellow}}`c ps1_git_details`'             \
          '{{fbred}}`c ps1_last_rc_show`{{reset}}'      \
          '\n\$ '
        end

        subs = { "uname" => ps1_helper.uname_wrap }
        %w[blink fbcyan fbgreen fbmagenta fbred fbwhite fbyellow nblink reset].each do |label|
          subs[label] = color_escape([label])
        end

        ps1 = template.gsub(/\{\{\w+\}\}/) { |str| subs[str[2..-3]] }

        "echo #{ps1.shellescape}"
      end
    end

    add :ps1_git_details, "Display super abbreviated current git repo info", ->(*args) do
      ps1_helper.x_git = cc(:x, "git").ok? if ps1_helper.x_git.nil?
      ps1_helper.x_git or return nil

      branch = cc(:gb).line or return nil
      status = bash("git status").lines
      bits = +""
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
      (rc = ENV["THE_C_LAST_RC"]) and ps1_helper.last_rc = rc
      nil
    end

    add :ps1_last_rc_show, "Display the last command exit code, saved before", ->(*args) do
      rc = ps1_helper.last_rc
      (rc && rc != "0") ? "echo \\ #{rc}" : nil
    end

    add :psg, "Find processes", ->(*args) do
      regex = Regexp.new(args[0] || "^", "i")
      lines = begin
        bash("ps -ewwH -o sid,pgid,ppid,pid,uid,tty,stat,cmd").lines
          .map.with_index do |ln, i|
            if i == 0
              color_escape(%W[fbgreen #{ln} reset])
            elsif regex.match?(ln)
              color_matches(ln, regex, "fbyellow")
            else
              nil
            end
          end.compact
      end
      page("+G") { |io| io.puts lines.join("\n") }
    end

    add :psync, "Sync abs pathnames from current / to matched ones below ROOT/ subdirs", ->(*args) do
      roots = cc(:roots, "ROOT").lines.map { "#{_1}/ROOT" }
      if roots.empty?
        puterr "Abs paths to sync must be created below 'ROOT/' subdirs."
        return false
      end

      page do |io|
        for root in roots
          io.puts "\n#{"_" * 80}\nProcess relative root: #{root.inspect}"

          paths = strout { |sio| Ffg::Cli.new([root, "."], io: sio).run }.split("\n").sort
          paths.each do |path|
            apath = path[root.size .. -1]

            if ! File.exist?(apath)
              io.puts "Warning: Not extant:\t#{apath}"
              next
            elsif ! File.readable?(apath)
              io.puts "Warning: Not readable:\t#{apath}"
              next
            end

            eapath, epath = [apath, path].map(&:shellescape)
            if bash("cmp -s #{eapath} #{epath}").ok?
              io.puts "Unchanged:\t\t#{apath}"
              next
            end

            io.puts "Syncing:\t\t#{apath}"
            bash("cp #{eapath} #{epath}", errs: io).ok? or return false
          end
        end
        true
      end
    end

    add :rce, "Edit ~/code/bash/bashrc", ->(*args) do
      c(:e, "#{ENV["HOME"]}/code/bash/bashrc")
    end

    add :rcp, "Propagate .bashrc to servers", ->(*args) do
      args.empty? and args = %w[
        dan@homer:.bashrc pi@homer:.bashrc root@homer:.bashrc
        dan@missybook:.bashrc root@missybook:.bashrc
        dvr@dvr:.bashrc root@dvr:.bashrc
      ]
      sources = %w[.bashrc .bashrc.the_c].map { "#{ENV["HOME"]}/#{_1}" }.select { File.exist?(_1) }
      targets = args.map do |target|
        target = "#{target}:hig.bashrc" if ! target.include?(":")
        sources.map { target + File.extname(_1) }
      end
      width = targets.flatten.map(&:size).max
      ok = true
      targets.each do |tuple|
        sources.zip(tuple).each do |source, target|
          print("#{target} ".ljust(width + 4, ".") + " ")
          ok &= ok2 = bash("scp -q #{source.shellescape} #{target.shellescape}", :errs).ok?
          puts "OK." if ok2
        end
      end
      ok
    end

    add :rh, "Recursive sha256 hash", ->(*args) do
      usage = -> do
        puterr "\nUsage: rh [{--sort-hash|-H}|{--sort-path|-P}|{--sort-size|-S}] [[{--cd|-d} DIR] PATHNAME ...] ...\n"
        false
      end
      sort = nil; cwd = "."; paths = Hash.new { |h, k| h[k] = [] }; idx = -1
      while (arg = args[idx += 1])
        c = 0
        arg =~ /^-[^-]*[h?]|^--help$/   && c = 1 and return usage[]
        arg =~ /^-[^-]*H|^--sort-hash$/ && c = 1 and sort = :hash
        arg =~ /^-[^-]*P|^--sort-path$/ && c = 1 and sort = :path
        arg =~ /^-[^-]*S|^--sort-size$/ && c = 1 and sort = :size
        arg =~ /^-[^-]*d|^--cd$/        && c = 1 and cwd = args[idx += 1]
        c == 0 and paths[cwd] << arg
      end
      paths["."].concat(Dir.children(".")) if paths.empty?

      TheC::RecursiveHash.new(paths, sort).run
      true
    end

    add :rl, "Recursive listing of files or dirs, sortable by date or size or count", ->(*args) do
      page { |io| RecursiveList.new.run(args, io: io) }
    end

    add :roots, "Select roots of dir trees containing a named subdir", ->(*args) do
      dirs = strout { |io| Roots.new.run(args, io: io) }
      dirs = dirs.inspect.gsub("$", "\\\\$") # Safe one-line value, which `.shellescape` does not make
      "echo -en #{dirs}"
    end

    add :sce, "Edit ~/.ssh/config", ->(*args) do
      c(:e, "#{ENV["HOME"]}/.ssh/config")
    end

    add :scl, "List ~/.ssh/config defined hosts", ->(*args) do
      matches = begin
        File.read("#{ENV["HOME"]}/.ssh/config")
          .lines
          .grep(/^\s*Host\b/)
          .map(&:lstrip)
          .sort
      end

      page { |io| io.puts matches.join }
    end

    add :setpath, "Clever path manipulator, guarantees proper ordering and deduping", ->(*args) do
      SetPath.new.run(args) # Returns bash code to eval
    end

    add :sudo, "Run sudo only if needed", ->(*args) do
      if cc(:x, "sudo").ok? && ENV["USER"] != "root"
        args = %w[sudo] + args
      end
      args.shelljoin
    end

    add :u, "Run system updater", ->(*args) do
      @upd ||= %w[pacman yum apt apt-get].detect { cc(:x, _1).ok? }

      if ! @upd
        puterr "No system updater found."
        return false
      end

      result = nil
      if @upd == "pacman"
        if args[0] == "c"
          result = "#{c :sudo} pacman -Sy && pacman -Qu #{args[1..-1].shelljoin} | #{c :m}"
        elsif args[0][0..1] == "-Q"
          result = "pacman #{args.shelljoin} | #{c :m}"
        end
      end
      result || c(:sudo, @upd, *args)
    end

    add :ua, "Run yay", ->(*args) do
      if args[0] == "c"
        args = %w[-Quya]
      else
        args = args.map { _1 =~ /^-([A-Z]([^a]+)?)$/ ? "-#{$~[1]}a" : _1 }
      end
      "yay #{args.shelljoin}"
    end

    add :ui, "Arch package details, --nn for max depth", ->(*args) do
      depth = "2"
      page do |io|
        args.each do |arg|
          if arg =~ /^--(\d+)$/
            depth = $~[1]
            next
          end
          script = "#{c :u, "-Si", arg}; #{c :u, "-Qi", arg}"
          out = bash(script).lines.map(&:rstrip).reject(&:empty?).uniq.join("\n")
          io.puts "\n#{out}"
          bash("pactree -rd#{depth} #{arg.shellescape}", echo: io)
        end
      end
    end

    add :vb, "View binary files", ->(*args) do
      cmd = "od -Ad -tx1z -w30"

      if args.empty? || args.member?("-")
        "#{cmd} - | #{c :m}"
      else
        page do |io|
          args.each do |file|
            io << "\n+ #{file.inspect}\n" if args.size > 1
            bash("#{cmd} #{file.shellescape}", echo: io)
          end
        end
      end
    end

    add :x, "Are all given commands executable", ->(*args) do
      args.all? { |c| bash("type #{c.shellescape}").ok? }
    end

    add :zzz, "Fallback base case if subcommand is unknown", ->(*args) do
      tbl = {
        idxs: 0...3,
        cmd:  %w[ git  svn cvs],
        dir:  %w[.git .svn CVS]
      }

      idx = tbl[:idxs].detect { |i| cc(:x, tbl[:cmd][i]).ok? && cc(:proot, tbl[:dir][i]).line }

      if idx
        "#{tbl[:cmd][idx]} #{args.shelljoin}"
      else
        puterr "Base: Cannot determine if current dir is git, svn, or cvs. Exiting."
        false
      end
    end
  end
end
end
