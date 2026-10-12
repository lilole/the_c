# frozen_string_literal: true

require "json"
require "shellwords"

module TheC
module Shortcuts
  ## The actual shortcut definitions.
   #
  class Core
    include Dsl
    include Helpers

    add :at_home, "Return success if the current machine is Dan's home one" do |sc|
      "[[ $HOSTNAME == danbook.danamis.com ]]"
    end

    add :at_work, "Return success if the current machine is Dan's work one" do |sc|
      "[[ $HOSTNAME == zzzdhiggins-vm ]]"
    end

    add :be, "Shorthand 'bundle exec'" do |sc|
      "bundle exec #{sc.args.shelljoin}"
    end

    add :cole, "Join given strings with ANSI color seqs" do |sc|
      return "echo #{color_escape(sc.args).shellescape}" if sc.args.any?

      keys  = @color_escape.keys
      width = keys.map(&:size).max
      list  = begin
        keys.each_slice(4).map { |codes|
          codes.map { |code|
            tail = code == codes.last ? "" : "," + " " * (width + 1 - code.size)
            "#{code}#{tail}"
          }.join
        }.join(",\n" + " " * 6)
      end

      usage = (<<~END).sub("{{LIST}}", list)
        Usage:
          cole {CODE|STRING} ...
        Where:
          CODE => One of:
              {{LIST}}
          STRING => Not a CODE, any other String value to include.
      END
      sc.stderr << usage
      "false"
    end

    add :d, "Change dirs with various abbreviations" do |sc|
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

      dir = (sc.args[0] =~ /^u(\d+)$/) ? ([".."] * $~[1].to_i).join("/") : map[sc.args[0]]
      if ! dir
        tmpfile = sc.tmpfile { |io|
          width = map.values.map(&:size).max
          io << "Valid abbrevs:\n"
          map.sort { |a, b| a[1] <=> b[1] }.each { |abbrev, dir|
            io << "  %s %s\n" % ["#{dir} ".ljust(width + 4, "."), abbrev]
          }
          io << "Examples:\n  $ c d r mp4* # cd to matched subdir\n  $ ls -l `c d v` # List without cd\n"
        }
        return "cat >&2 #{tmpfile}"
      end

      subs = sc.args[1..-1].join("/")
      subs.empty? or dir = Dir.glob("#{dir}/#{subs}").first || "#{dir}/#{subs}"

      "cd #{dir.shellescape} && pwd"
    end

    add :dc, "File diff with colors and paging" do |sc|
      "diff -U5 --color=always #{sc.args.shelljoin} | #{page_cmd}"
    end

    add :dns, "Test homer DNS server" do |sc|
      return "echo >&2 'c dns: Must be at home' && false" if cc(:at_home).fail?

      name = sc.args[0] || "goo.gl"
      hr = "_" * 80

      tmpfile = sc.tmpfile { |io|
        catch(:stop) {
          for rec in %w[A AAAA]
            for ipv, server in [%w[4 192.168.0.2], %w[6 2002:d1f0:3363:10:dea6:32ff:fe18:36db]]
              for flag in %w[notcp tcp]
                io << "\n#{hr}\n+ dig #{args}\n"
                args = %W[dig #{name} #{rec} +#{flag} -#{ipv} @#{server}]
                ok = system(*args, out: io, err: io)
                throw :stop if ! ok
              end
            end
          end
        }
      }
      "#{page_cmd} #{tmpfile}"
    end

    add :ds, "Create a new Docker container as a bg service" do |sc|
      sc.puts "\nName:  ", nl: :none; name  = sc.gets.chomp
      sc.puts "\nImage: ", nl: :none; image = sc.gets.chomp
      ports = []
      until nil
        sc.puts "\nPort maps (HOST:CONTAINER[/udp], - when done): ", nl: :none
        v = sc.gets.chomp
        break if v == "-"
        ports << v
      end
      envs = []
      until nil
        sc.puts "\nEnv var (NAME='VALUE', - when done): ", nl: :none
        v = sc.gets.chomp
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
      sc.puts "\nRun: #{args.shelljoin}"
      ask_continue("OK?", "yn") ? args.shelljoin : "false"
    end

    add :e, "Start the preferred editor if given an arg, otherwise just print the editor command" do |sc|
      editors = [
        # In order of preference
        { exe: "vscodium-electron", skip_if_no_params: true },
        { exe: "vim" },
        { exe: "vi" }
      ]
      found = editors.detect { |props|
        if    sc.args.none? && props[:skip_if_no_params] then false
        elsif executable?(props[:exe])                   then true
        else  false
        end
      }
      if ! found
        "false"
      elsif sc.args.any?
        args = sc.args.map { |a| File.realpath(a) rescue a }
        [found[:exe], found[:args_if_params], args.shelljoin].compact.join(" ")
      else
        args = [found[:exe], found[:args_if_no_params]].compact
        "echo #{args.shelljoin}"
      end
    end

    add :ff, "Find paths matching regex" do |sc|
      tmpfile = sc.tmpfile { |io|
        Thread.new { Ffg::Cli.new(sc.args, io: io).run }
      }
      sleep 0.075 until File.readable?(tmpfile)
      "#{pager_cmd} #{tmpfile}"
    end

    add :fg, "Find regex in files" do |sc|
      tmpfile = sc.tmpfile { |io|
        Thread.new { Ffg::Cli.new(%w[--fg].concat(sc.args), io: io).run }
      }
      sleep 0.075 until File.readable?(tmpfile)
      "#{pager_cmd} #{tmpfile}"
    end

    add :gb, "Print current branch, or match first regex branch" do |sc|
      usage = ->(msg) { raise "#{msg}: Usage: gb [-v] [regex]" }
      re = nil; verbose = false
      sc.args.each { |arg|
        if    arg =~ /^-[^-]*[h?]|^--help$/ then usage["Online help"]
        elsif arg == "-v"                   then verbose = true
        elsif ! re                          then re = arg
        else  usage["Invalid arg: #{arg.inspect}"]
        end
      }
      brs = bash("git branch").lines.grep(re ? Regexp.new(re) : /^\*/) # Current branch if no user re
      brs.map! { |ln| ln[2..-1] }
      br = brs[0]
      if brs.size == 1 && br != "(no branch)" # Success iff exactly 1 match
        "echo #{br.shellescape}"
      else
        verbose and sc.pute "Branch regex '#{re}' did not match exactly once: #{brs.inspect}"
        "false"
      end
    end

    add :gcb, "Checkout first branch whose name matches given regex" do |sc|
      (re = sc.args[0]) or raise "Regex is required."
      if (branch = cc(:gb, "-v", re, :errs).line)
        "git checkout #{branch.shellescape}"
      else
        "false"
      end
    end

    add :gd, "Smarter git diff, includes stats for each file" do |sc|
      opts = %w[--color=always]; paths = []; force = false
      sc.args.each { |arg|
        if    arg == "--"                             then force = true; next
        elsif arg[0] == "-" && ! force                then opts << arg
        elsif File.directory?(arg) || File.file?(arg) then paths << arg
        else  sc.puts "Warning: Ignoring arg: #{arg.inspect}"
        end
      }
      paths.empty? and paths << "."

      base = "git diff --relative"
      file_diff = ->(file) {
        o = opts.shelljoin; f = file.shellescape
        sc.puts ""
        bash("#{base} #{o} #{f} && echo && #{base} --stat=500 #{o} #{f}", echo: sc.stdout)
      }

      paths.each { |path|
        if File.file?(path)
          file_diff[path]
        else # Dir
          lines = bash("#{base} --stat=500 #{path.shellescape}").lines
          summary = lines.last
          lines[0..-2].each { |line|
            file = line.split(/ +\| /)[0]&.[](1..-1)
            if file && ! file.empty? && File.file?(file)
              file_diff[file]
            else
              sc.puts "Warning: Ignoring output word: #{file.inspect}"
            end
          }
          sc.puts "\nDir: #{path.inspect}: #{summary}"
        end
      }
      "true"
    end

    add :gdb, "Delete branch matching regex, with confirmation" do |sc|
      usage = ->(msg) { raise "#{msg}: Usage: gdb [--local|-l] REGEX" }
      local = false; re = nil
      sc.args.each { |arg|
        if    arg =~ /^-[^-]*l|^--local$/ then local = true
        elsif ! re then re = arg
        else  usage["Invalid arg: #{arg.inspect}"]
        end
      }
      usage["Regex is required"] if ! re

      br = cc(:gb, "-v", re, :errs).line or return "false"
      cmds = []
      cmds << "git push origin :#{br.shellescape}" if ! local
      cmds << "git branch -D #{br.shellescape}"
      cmds << "git fetch --prune"

      return "echo Skipped. && false" if ! ask_continue("Run: #{cmds.join(" && ")} ?", "yn")
      cmds.each { |cmd|
        sc.puts "\n+ #{cmd}"
        ran = bash(cmd)
        sc.puts ran.out
        return "false" if ran.fail?
      }
      "true"
    end

    add :gg, "Git gui" do |sc|
      '({ out="$(meld . 2>&1)" || echo "$out"; } &)'
    end

    add :glb, "List branches matching regex, or all by default" do |sc|
      re = Regexp.new(sc.args.shift || "^")
      dirs = sc.args.any? ? sc.args : ["."]
      tmpfile = sc.tmpfile { |io|
        Thread.new {
          find_git_workspaces(dirs).each { |dir|
            Dir.chdir(dir) {
              ran = bash("git branch -vv")
              lines = ran.ok? ? ran.lines.grep(re).join("\n") : ran.out
              lines = "(No matches.)" if lines.empty?
              io.puts "\n#{dir}\n#{lines}"
            }
          }
        }
      }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :gmm, "Refresh and merge regex match of ARGV[0] or master/main down to current branch" do |sc|
      this_br = cc(:gb, "-v", :errs).line
      return "false" if ! this_br

      if (other_br = args[0])
        other_br = cc(:gb, "-v", other_br, :errs).line
        return "false" if ! other_br
      else
        other_br = cc(:gb, "-v", " (master|main)$").line
        if ! other_br
          sc.puts "Cannot find default branch."
          return "false"
        end
      end

      sc.puts "Merging #{other_br.inspect} down to #{this_br.inspect}..."
      [
        "git pull",
        "git checkout #{other_br.shellescape}", "git pull",
        "git checkout #{this_br.shellescape}",
        "git merge -X ignore-space-change #{other_br.shellescape}"
      ].each { |cmd|
        sc.puts "\n+ #{cmd}"
        bash(cmd, echo: sc.stdout).ok? or return "false"
      }
      "true"
    end

    add :gmu, "Refresh and merge current branch up to regex match of ARGV[0] branch" do |sc|
      if ! (other_br = sc.args[0])
        sc.puts "Upper branch name is required."
        return "false"
      end

      other_br = cc(:gb, "-v", other_br, :errs).line
      return "false" if ! other_br

      this_br = cc(:gb, "-v", :errs).line
      return "false" if ! this_br

      sc.puts "Merging #{this_br.inspect} up to #{other_br.inspect}..."
      [
        "git pull",
        "git checkout #{other_br.shellescape}", "git pull",
        "git merge -X ignore-space-change #{this_br.shellescape}"
      ].each { |cmd|
        sc.puts "\n+ #{cmd}"
        bash(cmd, echo: sc.stdout).ok? or return "false"
      }
      "true"
    end

    add :gnb, "Create new branch named ARGV[0] off of regex match of ARGV[1] or current branch" do |sc|
      if ! (new_br = sc.args[0])
        sc.puts "Usage: gnb NEW_BRANCH [BASE]"
        return "false"
      end

      if (base = args[1])
        base = cc(:gb, "-v", base, :errs).line
      else
        base = cc(:gb, "-v", :errs).line
      end
      return "false" if ! base

      sc.puts "Creating new branch #{new_br.inspect} based on #{base.inspect}..."
      [
        "git pull",
        "git checkout #{base.shellescape}", "git pull",
        "git checkout -b #{new_br.shellescape}",
        "git push -u origin HEAD"
      ].each { |cmd|
        sc.puts "\n+ #{cmd}"
        bash(cmd, echo: sc.stdout).ok? or return false
      }
      "true"
    end

    add :gs, "Smarter git status, handles git subdirs" do |sc|
      usage = ->(msg) { sc.pute "#{msg}: Usage: gs [--quiet|-q] [--] DIR ..."; "false" }
      dirs = []; verbose = true; force = false; argi = -1
      sc.args.each { |arg|
        arg == "--" and (force = true; next)
        if arg[0] == "-" && ! force
          c = 0; i, a = arg[0..1] == "--" ? [arg.size, 0] : [1, 1] # TODO: Make this standard boilerplate
          arg =~ /^-[^-]*q|^--quiet$/ && c += i and verbose = false
          return usage["Invalid opt: #{arg.inspect}"] if c < arg.size - a
        else
          dirs << arg
        end
      }
      if dirs.empty?
        raise "Cannot find workspace root" if ! (arg = cc(:proot, ".git").line)
        dirs << arg
      end

      tmpfile = sc.tmpfile { |io| Thread.new {
        cwd_re = Regexp.escape(Dir.pwd)
        files = Hash.new { |h, k| h[k] = [] }
        dirs.each { |dir|
          Dir.chdir(dir) {
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
          }
        }
      } }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :gu, "Smarter git update, handles git subdirs" do |sc|
      usage = ->(msg) { sc.pute "#{msg}: Usage: gu [--other-branch|-b] [[--dir|-d] DIR] ..."; "false" }
      dirs = []; other_branch = nil; argi = -1
      while (arg = sc.args[argi += 1])
        arg =~ /^-[^-]*[h?]|^--help$/ and return usage["Online help"]
        if arg[0] == "-"
          c = 0; i, a = arg[0..1] == "--" ? [arg.size, 0] : [1, 1] # TODO: Helper method for this
          arg =~ /^-[^-]*b|^--other-branch$/ && c += i and other_branch = args[argi += 1]
          arg =~ /^-[^-]*d|^--dir$/          && c += i and dirs << args[argi += 1]
          return usage["Invalid opt: #{arg.inspect}"] if c < arg.size - a
        else
          dirs << arg
        end
      end
      dirs << "." if dirs.empty?

      tmpfile = sc.tmpfile { |io| Thread.new {
        find_git_workspaces(dirs).each { |dir|
          Dir.chdir(dir) {
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
          }
        }
      } }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :h, "Dan's safe history wrapper" do |sc|
      usage = -> { sc.pute "Usage: h {s[ave]|l[oad]}"; "false" }
      if sc.args[0] =~ /^s/
        flags = %w[a]
        msg = "History saved to '$HISTFILE'."
      elsif sc.args[0] =~ /^l/
        flags = %w[a c r]
        msg = "History loaded from '$HISTFILE'."
      else
        return usage[]
      end
      cmd = flags.map { |f| "history -#{f}" }.join(" && ")
      "#{cmd} && echo #{msg.shellescape}"
    end

    add :hum, "Stop hum noise on the Alienware machine" do |sc|
      "echo 0 | #{c :sudo, "tee", "/sys/module/snd_hda_intel/parameters/power_save"}"
    end

    add :jc, "Run journalctl our way" do |sc|
      less = "#{less_opts} +G"
      args = %W[env SYSTEMD_PAGER=less SYSTEMD_LESS=#{less} journalctl --no-hostname] + sc.args
      args.member?("--user") ? args.shelljoin : c(:sudo, *args)
    end

    add :l, "Run ls the preferred way" do |sc|
      "LANG=C ls -alF --group-directories-first --block-size=\\'1 --color=always #{sc.args.shelljoin}" \
        " |& #{c :m}"
    end

    add :lc, "Load predefined text into clipboard" do |sc|
      clips = {
        "rb0" => (<<~END),
          # frozen_string_literal: true
          #
          # Copyright 2026 Dan Higgins
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

      name = sc.args[0]; clip = clips[name]
      if ! clip
        sc.pute "Usage: CLIP_NAME\nWhere: CLIP_NAME <= #{clips.keys.sort}"
        return "false"
      end

      one_line = clip.inspect.gsub("$", "\\\\$") # We need this because `.shellescape` leaves LFs

      "echo -e #{one_line} | xclip -in -rmlastnl -selection clipboard" \
        " && echo #{"Loaded clip: #{name.inspect}".shellescape}"
    end

    add :less_options, "The standard opts for `less`" do |sc|
      "echo #{less_opts.shellescape}"
    end

    add :m, "Run 'more' style viewer the preferred way" do |sc|
      "less #{less_opts} #{sc.args.shelljoin}"
    end

    add :mi, "Probe media info" do |sc|
      files = []; full = help = nil; idx = -1
      while (arg = sc.args[idx += 1])
        arg =~ /^-[^-]*[h?]|^--help$/ and help = true
        arg =~ /^-[^-]*f|^--full$/    and full = true
        arg[0] != "-" and files << arg
      end
      help ||= files.empty? || ! files.all? { File.readable?(_1) }
      if help
        sc.pute(<<~END)
          Usage: mi [--full|-f] FILE ...
          Where:
            -f, --full => Show all discovered media params as raw JSON.
                Default is to show common params.
        END
        return "false"
      end
      subject = {}
      detect = ->(*names) { subject[names.detect { |n| subject[n] }] || "?" }
      to_num = ->(*names) {
        s = detect[*names].strip
        f = (a, b = s[/\A([\d.]+)[^\d.]+([\d.]+)\z/, 1, 2].map(&:to_f); a / b) rescue Float::NAN
        f.nan? ? s.to_f : f
      }
      with_num = ->(*names) { "#{commafy(to_num[*names].round(3))} (#{detect[*names]})" }
      with_kbps = ->(*names) { "#{commafy((to_num[*names] / 1000).round)} Kbps (#{detect[*names]})" }
      with_time = ->(*names) { n = to_num[*names].round; "#{n / 60}m #{n % 60}s (#{detect[*names]})" }
      common = ->(json) {
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
      }
      script = "ffprobe -hide_banner -loglevel quiet -print_format json" \
        " -show_format -show_streams -show_chapters -analyzeduration #{4 * 3600 * 10**6}"
      tmpfile = sc.tmpfile { |io| Thread.new {
        for file in files
          io.puts "\n+ #{file.inspect}"; gb = 10**9
          sz = (v = File.size(file)) > 16*gb ? v / 4 : (v > 8*gb ? v / 2 : (v > 4*gb ? 4*gb : v))
          raw = `#{script} -probesize #{sz} #{file.shellescape}`.strip
          io.puts(full ? raw : common[raw])
        end
      } }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :need_screen, "Return success if on a tty at home" do |sc|
      "#{c :at_home} && #{c :x, "screen"}" \
        " && [[ ! $WINDIR && $USER == dan && $(tty) == /dev/tty[12] ]]"
    end

    add :need_x, "Return success if startx should be run" do |sc|
      "(#{c :at_home} || #{c :at_work})" \
        " && [[ ! $WINDIR && $USER == dan && $(tty) == /dev/tty1 ]]" \
        " && ! pgrep '^X(org)?$' &> /dev/null"
    end

    add :o, "Open files in their default viewer apps" do |sc|
      err = ->(msg) { sc.pute msg; "false" }
      return err["No files given."] if sc.args.empty?
      return err["Not configured for file viewer in this env."] if ! executable?("xdg-open")

      sc.args.each { |f| spawn("xdg-open #{f.shellescape}", out: sc.stdout, err: sc.stderr) }
      "true"
    end

    add :pd, "Dan's smart pushd/popd wrapper" do |sc|
      s = sc.args[0].to_s; n = s.to_i
      if    s.empty?       then "dirs"
      elsif s =~ /^-\d+$/  then "popd +#{-n - 1}"
      elsif s =~ /^\+\d+$/ then "pushd +#{n - 1}"
      else  "pushd #{s.shellescape}"
      end
    end

    add :proot, "Detect path to parent dir containing named subdir." do |sc|
      name = sc.args[0]
      raise "Name of subdir to search is required." if ! name

      parent = File.realpath(sc.args[1] || Dir.pwd)

      until File.directory?(File.join(parent, name))
        parent == "/" and (parent = ""; break)
        parent = File.dirname(parent)
      end

      "echo #{parent.shellescape}"
    end

    add :ps1, "Generate fancy xterm PS1 value" do |sc|
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
        %w[blink fbcyan fbgreen fbmagenta fbred fbwhite fbyellow nblink reset].each { |label|
          subs[label] = color_escape([label])
        }

        ps1 = template.gsub(/\{\{\w+\}\}/) { |str| subs[str[2..-3]] }

        "echo #{ps1.shellescape}"
      end
    end

    add :ps1_git_details, "Display super abbreviated current git repo info" do |sc|
      ps1_helper.x_git = executable?("git") if ps1_helper.x_git.nil?
      ps1_helper.x_git or return ":"

      branch = cc(:gb).line or return ":"
      status = bash("git status").lines
      bits = +""
      bits << "!" if status.any? { _1.include?("modified:") }
      bits << "x" if status.any? { _1.include?("deleted:") }
      bits << "?" if status.any? { _1.include?("Untracked files") }
      bits << "+" if status.any? { _1.include?("new file:") }
      bits << "*" if status.any? { _1.include?("Your branch is ahead of") }
      bits << ">" if status.any? { _1.include?("renamed:") }
      bits.empty? or bits.insert(0, " ")
      "echo #{" [#{branch}#{bits}]".shellescape}"
    end

    add :ps1_last_rc_save, "Save the last command exit code in a shared place for later" do |sc|
      (rc = ENV["THE_C_LAST_RC"]) and ps1_helper.last_rc = rc
      ":"
    end

    add :ps1_last_rc_show, "Display the last command exit code, saved before" do |sc|
      rc = ps1_helper.last_rc
      (rc && rc != "0") ? "echo \\ #{rc}" : ":"
    end

    add :psg, "Find processes" do |sc|
      regex = Regexp.new(sc.args[0] || "^", "i")
      lines = begin
        bash("ps -ewwH -o sid,pgid,ppid,pid,uid,tty,stat,cmd").lines
        .map.with_index { |ln, i|
          if i == 0
            color_escape(%W[fbgreen #{ln} reset])
          elsif regex.match?(ln)
            color_matches(ln, regex, "fbyellow")
          else
            nil
          end
        }.compact
      end
      tmpfile = sc.tmpfile { |io| io.puts lines.join("\n") }
      "#{c :m} +G #{tmpfile}"
    end

    add :psync, "Sync abs pathnames from current / to matched ones below ROOT/ subdirs" do |sc|
      roots = cc(:roots, "ROOT").lines.map { "#{_1}/ROOT" }
      if roots.empty?
        sc.pute "Abs paths to sync must be created below 'ROOT/' subdirs."
        return "false"
      end

      tmpfile = sc.tmpfile { |io|
        for root in roots
          io.puts "\n#{"_" * 80}\nProcess relative root: #{root.inspect}"

          paths = strout { |sio| Ffg::Cli.new([root, "."], io: sio).run }.split("\n").sort
          paths.each { |path|
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
            bash("cp #{eapath} #{epath}", errs: io).ok? or return "false"
          }
        end
      }
      "#{page_cmd} #{tmpfile}"
    end

    add :rce, "Edit ~/code/bash/bashrc" do |sc|
      c(:e, "#{ENV["HOME"]}/code/bash/bashrc")
    end

    add :rcp, "Propagate .bashrc to servers" do |sc|
      (args = sc.args).empty? and args = %w[
        dan@homer:.bashrc pi@homer:.bashrc root@homer:.bashrc
        dan@missybook:.bashrc root@missybook:.bashrc
        dvr@dvr:.bashrc root@dvr:.bashrc
      ]
      sources = %w[.bashrc .bashrc.the_c].map { "#{ENV["HOME"]}/#{_1}" }.select { File.exist?(_1) }
      targets = args.map { |target|
        target = "#{target}:hig.bashrc" if ! target.include?(":")
        sources.map { target + File.extname(_1) }
      }
      width = targets.flatten.map(&:size).max
      ok = true
      targets.each { |tuple|
        sources.zip(tuple).each { |source, target|
          sc.puts("#{target} ".ljust(width + 4, ".") + " ", nl: :none)
          ok2 = bash("scp -q #{source.shellescape} #{target.shellescape}", errs: sc.stdout).ok?
          sc.puts "OK." if ok2
          ok &&= ok2
        }
      }
      ok.to_s
    end

    add :rh, "Recursive sha256 hash" do |sc|
      usage = -> {
        sc.pute "\nUsage: rh [{--sort-hash|-H}|{--sort-path|-P}|{--sort-size|-S}] [[{--cd|-d} DIR] PATHNAME ...] ...\n"
        "false"
      }
      sort = nil; cwd = "."; paths = Hash.new { |h, k| h[k] = [] }; idx = -1
      while (arg = sc.args[idx += 1])
        c = 0
        arg =~ /^-[^-]*[h?]|^--help$/   && c = 1 and return usage[]
        arg =~ /^-[^-]*H|^--sort-hash$/ && c = 1 and sort = :hash
        arg =~ /^-[^-]*P|^--sort-path$/ && c = 1 and sort = :path
        arg =~ /^-[^-]*S|^--sort-size$/ && c = 1 and sort = :size
        arg =~ /^-[^-]*d|^--cd$/        && c = 1 and cwd = sc.args[idx += 1]
        c == 0 and paths[cwd] << arg
      end
      paths["."].concat(Dir.children(".")) if paths.empty?

      TheC::RecursiveHash.new(paths, sort, sc.stdout).run
      "true"
    end

    add :rl, "Recursive listing of files or dirs, sortable by date or size or count" do |sc|
      tmpfile = sc.tmpfile { |io|
        Thread.new {
          RecursiveList.new.run(sc.args, io: io)
        }
      }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :roots, "Select roots of dir trees containing a named subdir" do |sc|
      dirs = strout { |io| Roots.new.run(sc.args, io: io) }
      dirs = dirs.inspect.gsub("$", "\\\\$") # Safe one-line value, which `.shellescape` does not make
      "echo -en #{dirs}"
    end

    add :sce, "Edit ~/.ssh/config" do |sc|
      c(:e, "#{ENV["HOME"]}/.ssh/config")
    end

    add :scl, "List ~/.ssh/config defined hosts" do |sc|
      matches = begin
        File.read("#{ENV["HOME"]}/.ssh/config")
        .lines
        .grep(/^\s*Host\b/)
        .map(&:lstrip)
        .sort
      end
      tmpfile = sc.tmpfile { |io| io.puts matches.join }
      "#{page_cmd} #{tmpfile}"
    end

    add :setpath, "Clever path manipulator, guarantees proper ordering and deduping" do |sc|
      SetPath.new.run(sc.args) # Returns bash code to eval
    end

    add :sudo, "Run sudo only if needed" do |sc|
      sc.args = %w[sudo] + sc.args if executable?("sudo") && ENV["USER"] != "root"
      sc.args.shelljoin
    end

    add :u, "Run system updater" do |sc|
      @upd ||= %w[pacman dnf yum apt apt-get].detect { executable?(_1) }

      if ! @upd
        sc.pute "No system updater found."
        return "false"
      end

      result = nil
      if @upd == "pacman"
        if sc.args[0] == "c"
          result = "#{c :sudo} pacman -Sy && pacman -Qu #{sc.args[1..-1].shelljoin} | #{c :m}"
        elsif sc.args[0][0..1] == "-Q"
          result = "pacman #{sc.args.shelljoin} | #{c :m}"
        end
      end
      result || c(:sudo, @upd, *sc.args)
    end

    add :ua, "Run yay" do |sc|
      if sc.args[0] == "c"
        sc.args = %w[-Quya]
      else
        sc.args = sc.args.map { _1 =~ /^-([A-Z]([^a]+)?)$/ ? "-#{$~[1]}a" : _1 }
      end
      "yay #{sc.args.shelljoin}"
    end

    add :ui, "Arch package details, --nn for max depth" do |sc|
      depth = "2"
      tmpfile = sc.tmpfile { |io| Thread.new {
        sc.args.each { |arg|
          if arg =~ /^--(\d+)$/
            depth = $~[1]
            next
          end
          script = "#{c :u, "-Si", arg}; #{c :u, "-Qi", arg}"
          out = bash(script).lines.map(&:rstrip).reject(&:empty?).uniq.join("\n")
          io.puts "\n#{out}"
          bash("pactree -rd#{depth} #{arg.shellescape}", echo: io)
        }
      } }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :vb, "View binary files" do |sc|
      cmd = "od -Ad -tx1z -w30"
      return "#{cmd} - | #{c :m}" if sc.args.empty? || sc.args.member?("-")
      tmpfile = sc.tmpfile { |io|
        Thread.new {
          sc.args.each { |file|
            io << "\n+ #{file.inspect}\n" if args.size > 1
            bash("#{cmd} #{file.shellescape}", echo: io)
          }
        }
      }
      sleep 0.075 until File.readable?(tmpfile)
      "#{page_cmd} #{tmpfile}"
    end

    add :x, "Are all given commands executable" do |sc|
      sc.args.all? { |c| executable?(c) }
    end

    add :zzz, "Fallback base case if subcommand is unknown" do |sc|
      tbl = [
        { cmd: "git", dir: ".git" },
        { cmd: "svn", dir: ".svn" },
        { cmd: "cvs", dir: "CVS" }
      ]
      row = tbl.detect { |r| executable?(r[:cmd]) && cc(:proot, r[:dir]).line }
      if row
        "#{row[:cmd]} #{sc.args.shelljoin}"
      else
        sc.pute "Base: Cannot determine if current dir is git, svn, or cvs. Exiting."
        "false"
      end
    end
  end
end
end
