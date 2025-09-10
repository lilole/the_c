#!/usr/bin/env ruby
# frozen_string_literal: true
#
# Copyright 2024-2025 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

## Every shell shortcut macro/script/tool in a single block of Ruby code.
 #
 # Notes:
 # - Requires Ruby 3.1+ and Linux.
 # - This is meant to be pasted into a bashrc to work alongside a custom "c"
 #   function. For an example see:
 #     https://github.com/lilole/the_c/blob/main/the_c.bashrc
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

require "digest/sha2"
require "find"
require "io/console"
require "json"
require "set"
require "shellwords"
require "stringio"

module TheC
  module Extensions
    def self.apply
      ::String.prepend String
    end

    module String
      ## Allow `some_string[/(foo)(bar).../, 1, 2, ...]` to return an array of all
       # the captures. All array elements may be `nil` if there is no match.
       #
      def [](*args)
        if Regexp === args[0] && args.size > 2
          rem = args[0].match(self)
          args[1..-1].map { |idx| rem&.[](idx) }
        else
          super
        end
      end
    end # String
  end # Extensions

  Extensions.apply

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
    end # Info

    ## Easy ways to run `bash` scripts with full control of IO.
     #
    module Bash
      include Info

      Result = Struct.new(:exitcode, :fail?, :line, :lines, :ok?, :okout, :out, :stderr, :stdout)

      ## Run `bash` with input as a script, and return an object with useful
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
        echo_to ||= $stdout if opts.member?(:echo)
        errs_to ||= $stderr if opts.member?(:errs)

        pipr_s, pipw_s = IO.pipe # Process's stdout redirect
        pipr_e, pipw_e = IO.pipe # Process's stderr redirect
        begin
          io_s, io_e, io_a = Array.new(3) { StringIO.new } # Capture stdout, stderr, stdout+stderr
          stile = Mutex.new

          start  = -> { spawn(["bash", "#{my_classname}-bash"], "-c", script, out: pipw_s, err: pipw_e) }
          finish = ->(_) { pipw_s.close; pipw_e.close }

          do_io = ->(io_in, io_out) do
            do_errs_to = errs_to && io_out == io_e
            while (line = io_in.gets)
              io_out.write(line)
              errs_to.write(line) if do_errs_to
              stile.synchronize do
                io_a.write(line)
                echo_to.write(line) if echo_to
              end
            end
          end

          process  = Thread.new { Process::Status.wait(start[]).tap(&finish) }
          reader_s = Thread.new { do_io[pipr_s, io_s] }
          reader_e = Thread.new { do_io[pipr_e, io_e] }

          stat = process.join.value
          [reader_s, reader_e].each { _1.join }
        ensure
          [pipr_s, pipw_s, pipr_e, pipw_e].each { _1.close }
        end

        ok = stat.success?
        out = { a: io_a, s: io_s, e: io_e }.map { |k, io| io.rewind; [k, io.read.chomp] }.to_h
        result = Result.new
        result.send("fail?=", ! ok)
        result.send("ok?=",   ok)
        result.exitcode = stat.exitstatus
        result.lines    = out[:s].split("\n")
        result.line     = result.lines.last # Maybe nil
        result.okout    = ok ? out[:a] : nil
        result.out      = out[:a]
        result.stderr   = out[:e]
        result.stdout   = out[:s]
        result
      end
    end # Bash

    ## Generic methods to output text.
     #
    module Output
      ## Shorthand for outputting error/warning/info messages on `$stderr` in
       # user's console.
       #
      def puterr(*strings, nolf: false)
        $stderr << strings.join
        $stderr << "\n" if ! nolf || strings.empty?
      end
    end # Output

    module Prompt
      include Output

      ## Prompt user for confirmation before continuing.
       # The `opts` may be any string of typable characters, with a single
       # uppercase to be the default if user presses Enter.
       # If the choice is "q", then exit immediately.
       # If the choice is "y" or "n", then return Boolean.
       # Any other choice returns the character.
       #
      def ask_continue(prompt="Continue?", opts="Ynq")
        def_reply = opts.gsub(/[^A-Z]+/, "")
        raise "Only 1 uppercase is allowed: #{opts.inspect}" if def_reply.size > 1
        puterr
        begin
          puterr "#{prompt} [#{opts}] ", nolf: true
          reply = $stdin.getch(intr: true).chomp
          reply = def_reply if reply.empty? && ! def_reply.empty?
          lreply = reply.downcase
          puterr lreply
        end until lreply =~ /^[#{opts.downcase}]$/
        puterr
        exit if lreply == "q"
        %w[y n].member?(lreply) ? lreply == "y" : reply
      end
    end # Prompt

    module TempFile
      include Info

      ## Start a single thread that cleans specific glob patterns that are older
       # than a specific number of secs, checking once per minute.
       # Use `add_to_clean` to add globs and ttls to be cleaned here.
       #
      if $TheC__Mixin__TempFile_state.nil?
        ->(state) do
          state[:to_clean_tups] = []
          state[:clean_thread] = begin
            Thread.new do
              loop do
                state[:to_clean_tups].each do |(ttl_secs, glob_pattern)|
                  cutoff = Time.now - ttl_secs
                  Dir.glob(glob_pattern, File::FNM_DOTMATCH).each do |file|
                    begin
                      File.delete(file) if File.mtime(file) <= cutoff
                    rescue
                      ## If we get here the `mtime` or `delete` failed, so it's
                       # already gone. This is fairly common because other processes
                       # will have this same thread with the same globs.
                      nil
                    end
                  end
                end
                sleep 60
              end
            end
          end
        end.call($TheC__Mixin__TempFile_state = {})
      end

      def self.add_to_clean(ttl_secs, glob_pattern)
        pairs = $TheC__Mixin__TempFile_state[:to_clean_tups]
        new_pair = [ttl_secs, glob_pattern]
        pairs << new_pair if ! pairs.member?(new_pair)
      end

      ## Create a unique temp file with optional given body, and return an object
       # with `write`, `path`, and `io` methods. Use the `write` method to add to
       # the file. When done writing, use the `path` method to close and return
       # the file's path. Use `io` for direct access to the open File object.
       #
       # A single thread in this process will monitor created tmp files and clean
       # any that were touched 30+ mins ago, each minute, from any process.
       #
      def make_temp_file(body=nil)
        cache = @make_temp_file_cache ||= begin
          result = {}
          result[:seq] = 0
          result[:template] = "#{fast_tmp_dir}/#{my_classname}-#{$$}-{{SEQ}}.tmp"
          result[:class] = Class.new do
            attr :io
            def initialize(path) = (@io = File.open(path, "w"); File.chmod(0600, path); io.sync = true)
            def write(data)      = io.write(data)
            def path             = (io.close; io.path)
          end
          TempFile.add_to_clean(30 * 60, "#{fast_tmp_dir}/#{my_classname}-*.tmp")
          result
        end

        begin
          seq = (cache[:seq] += 1)
          tmpfile = cache[:template].sub("{{SEQ}}", seq.to_s)
        end while File.exist?(tmpfile)

        tmpobj = cache[:class].new(tmpfile)
        tmpobj.write(body) if body
        tmpobj
      end
    end # TempFile

    module Text
      ## Convert a number to a String with thousands separators.
       #
      def commafy(n)
        s = String === n ? n.dup : n.to_s
        s.tap { |v| v.reverse!; v.gsub!(/(\d{3})(?=\d)(?!\d*\.)/, "\\1,"); v.reverse! }
      end
    end # Text

    ## Output text to a configured IO, with default `$stdout`.
     #
    module Say
      class << self
        attr_accessor :io
      end
      self.io = $stdout

      ## Join `strings` and output to current value of `Say.io`, iff it's set.
       #
      def say(*strings, nolf: false)
        return if ! Say.io
        Say.io << "++ #{self.class.name} #{$$}: " if ! nolf
        Say.io << strings.join
        Say.io << "\n" if ! nolf || strings.empty?
      end
    end # Say

    ## Track valid status.
     #
    module WeOk
      class AreWeOk
        def yes!(*args)
          if args.empty?
            if   Numeric === @ok then @ok += 1
            else @ok = true
            end
          elsif args.size == 1
            value = args[0]
            if   value == :now then @ok
            else @ok = value
            end
          else
            raise "Only 0 or 1 arg allowed"
          end

          self
        end

        def now = @ok

        def now?
          if   Numeric === @ok then @ok != 0
          else !! @ok
          end
        end
      end # AreWeOk

      ## Access current status object.
       #
      def are_we_ok = @are_we_ok ||= AreWeOk.new

      ## With an arg, set the current status, to either Numeric or boolean.
       # With no arg, increment current status if Numeric, or reset to true.
       #
      def ok!(*args) = are_we_ok.yes!(*args)

      ## Read current status. Numeric is ok if > 0.
       #
      def ok? = are_we_ok.now?
    end # WeOk

    ## Generic utility methods which may be included in any class.
     # NONE of these methods read or write any context from their includers.
     #
    module Util
      include Bash
      include Info
      include Output
      include Prompt
      include TempFile
      include Text
    end # Util
  end # Mixin

  ## Access to `TheC::Mixin::Util` methods without including.
   #
  module Util
    extend TheC::Mixin::Util
  end # Util

  ## Manage a tree of file nodes.
   #
  class FileNode
    ## Configured filters when loading a node's subtree. An instance of this is
     # passed to any block given to `FileNode.initialize`.
     #
    class Filters
      attr_reader :only, :prune, :skip

      ## Allow only the given path matchers on the subtree. Returns self.
       # A Proc must receive the path, and return truthy if matched.
       # A Regexp must match the path.
       # A String must match the path's base name.
       #
      def only!(*names_res_or_procs) = (@only = names_res_or_procs.flatten; self)

      ## Allow no subtree for the given path matchers. Returns self.
       # See `#only!` for matcher details.
       #
      def prune!(*names_res_or_procs) = (@prune = names_res_or_procs.flatten; self)

      ## Skip the given path matchers on the subtree. Returns self.
       # See `#only!` for matcher details.
       #
      def skip!(*names_res_or_procs) = (@skip = names_res_or_procs.flatten; self)

      def prune?(path) = prune&.any? { |arg| match?(arg, path) }

      def skip?(path)
        only&.none? { |arg| match?(arg, path) } ||
          skip&.any? { |arg| match?(arg, path) }
      end

      def match?(arg, path)
        if    String === arg then arg == File.basename(path)
        elsif Regexp === arg then arg.match?(path)
        elsif Proc === arg   then !! arg[path]
        else  raise "Only Proc, Regexp, or String is valid: #{arg.inspect}"
        end
      end
    end # Filters

    attr_reader :children, :filters, :path, :stat

    ## Typical usage is a given pathname, and if any filtering is needed then
     # pass a block that calls any combination of `Filters#only!`,
     # `Filters#prune!`, and `Filters.skip!`.
     #
    def initialize(pathname, filters: nil, &filters_config)
      @path    = (pathname =~ %r`\A[/.]` ? +"" : +"./") << pathname.sub(%r`/\s*\z`, "")
      @filters = filters || Filters.new
      @children = []
      @stat     = File.lstat(path)
      yield(@filters) if filters_config
      load!
    end

    ## Convert entire node tree to a block of text lines.
     #
    def to_tree(level=0, indent_str: "+ ")
      ind = indent_str * level
      name = (level == 0) ? path.dup : File.basename(path)
      name << "/" if dir?
      text = +"#{ind}#{name}\n"
      text << children.sort.map { |child| child.to_tree(level + 1, indent_str: indent_str) }.join
    end

    ## The number of bytes in this node's subtree.
     #
    def size = @size ||= dir? ? (children.reduce(0) { |acc, child| acc + child.size }) : stat.size

    ## The number of files in this node's subtree. File nodes count as 1.
     #
    def count = @count ||= dir? ? (children.reduce(0) { |acc, child| acc + child.count }) : 1

    ## Convert the entire node tree to an array of pathnames.
     #
    def to_a = children.map { |child| child.to_a }.flatten << self

    ## Make default sort have directory nodes above file nodes.
     #
    def <=>(other)
      a, b = self, other
      a_dir = a.dir? ? 1 : 0
      b_dir = b.dir? ? 1 : 0
      2 * (a_dir <=> b_dir) +
        (a.path <=> b.path)
    end

    def dir? = stat.directory?

    private \
    def load!
      if dir? && ! filters.prune?(path)
        Dir.glob("#{path}/*", File::FNM_DOTMATCH).each do |sub_path|
          next if filters.skip?(sub_path) || sub_path.end_with?("/.") || sub_path.end_with?("/..")
          children << FileNode.new(sub_path, filters: filters)
        end
      end
      self
    end
  end # FileNode

  ## A rudimentary Ractor based on anonymous pipes.
   #
  class Practor
    attr_reader :actor_pid, :forked, :from_main_r, :from_main_w, :to_main_r, :to_main_w

    def initialize
      @from_main_r, @from_main_w = IO.pipe("ASCII-8BIT:ASCII-8BIT")
      @to_main_r,   @to_main_w   = IO.pipe("ASCII-8BIT:ASCII-8BIT")
    end

    def start(&block)
      @actor_pid = fork do
        @forked = true
        cap_unused_ends
        block[self]
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

    def finish
      from_main_w.close
    end
  end # Practor

  module Ffg
    VERSION = "24.1101"

    module Usage
      def usage(msg=nil, exit_code=1)
        say <<~END

        #{msg || "Online help."}

        Description:
          File Find or Grep v#{VERSION}: Grep with full Ruby Regexp capabilities for file paths or contents.

        Usage:
          (For file paths.)
          ffg [-imx] [--dot-dirs|-d] [--quiet|-q] [--source-tool-dirs|-s] [--other-devices|-D] \\
              [--no-skips|-N] [--skip|-n path_regex] ... [--] [dir ... file ...] regex

          (For file contents.)
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
      attr_accessor :ctxt, :dot_dirs, :fg, :keep_dev, :path_args, :path_only, :quiet, :re_arg, :re_opts,
        :skip_res, :src_dir_names, :src_dirs

      def initialize
        @ctxt = @re_opts = 0
        @dot_dirs = @fg = @path_only = @quiet = @src_dirs = false
        @keep_dev      = true
        @path_args     = []
        @skip_res      = %w[/(tmp|log|coverage|(r?spec|tests?)/fixtures)$]
        @src_dir_names = %w[.git .svn CVS].to_set
      end
    end # Configuration

    class Find
      attr_reader :cfg, :core

      def initialize(core, cfg)
        @core = core
        @cfg = cfg
      end

      def run
        core.standard_client_run do |_pool_member_id, files_queue, lines_queue|
          empty = [].freeze

          while (seq_path = files_queue.deq)
            path = seq_path[1].b

            if cfg.re_arg.match?(path)
              lines = ["#{path}\n"]
            else
              lines = empty
            end

            lines_queue.enq([seq_path[0], lines])
          end
        end
      end
    end # Find

    class Grep
      include TheC::Mixin::Say

      attr_reader :cfg, :core, :my_actors

      def initialize(core, cfg)
        @core = core
        @cfg = cfg
      end

      def run
        # Since Practor forks, we instantiate all of them first to try to keep it simple
        start_actors

        core.standard_client_run do |pool_member_id, files_queue, lines_queue|
          actor_idx = pool_member_id - 1
          my_actor = my_actors[actor_idx]
          while (seq_path = files_queue.deq)
            my_actor.send(seq_path[1])
            lines = my_actor.receive
            lines_queue.enq([seq_path[0], lines])
          end
          my_actor.finish
        end
      end

      def start_actors
        @my_actors = begin
          (1..core.file_processor_pool_size).map do
            TheC::Practor.new.start do |actor|
              loop do
                path = actor.receive or break
                actor.send(grep_file(path))
              end
            end
          end
        end
      end

      def grep_file(file_path)
        path = file_path.b # Needed in rescue block
        result = []
        File.open(file_path, "rt") do |f|
          ctxt_val = (cfg.ctxt > 0) ? cfg.ctxt : nil
          if ctxt_val # TODO: Classify logic for this
            ctxt_buf = []
            last_match_num = last_result_num = nil
          end

          num = 0
          while (num += 1; line = f.gets)
            matched = cfg.re_arg.match?(line.b.chomp)

            if cfg.path_only
              if matched
                result << path
                break
              end
              next
            end

            next unless ctxt_val || matched

            line = "%s:%03d: %s" % [path, num, line]
            if ctxt_val
              if matched
                # Output ctxt lines window and matched line
                result << "...\n" if last_result_num && num - 1 - ctxt_val > last_result_num
                result.concat(ctxt_buf) << line
                ctxt_buf.clear
                last_match_num = last_result_num = num
              elsif last_match_num && num - last_match_num <= ctxt_val
                # Output ctxt lines after last matched line
                result << line
                last_result_num = num
              else
                # Fill ctxt lines sliding window
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
        cfg.quiet or say "Warning: Skipping unreadable file: #{path}"
        []
      end
    end # Grep

    class Core
      include TheC::Mixin::Say

      attr_reader :cfg, :files_queue, :files_thread, :lines_queue, :lines_threads

      def initialize(cfg)
        @cfg = cfg
        @files_queue = Queue.new
        @lines_queue = Queue.new
      end

      def run
        if cfg.fg
          Grep.new(self, cfg).run
        else
          Find.new(self, cfg).run
        end
      end

      def standard_client_run(&client_file_processor)
        file_scanner_start
        begin
          file_processor_pool_start do |pool_member_id, files_queue, lines_queue|
            client_file_processor[pool_member_id, files_queue, lines_queue]
          end
          begin
            display_lines
          ensure
            file_processor_pool_finish
          end
        ensure
          file_scanner_finish
        end
      end

      def sub_files(dir_or_file, only_device=nil, &block)
        work = [dir_or_file] # Depth-first stack, no recursion needed baby
        begin
          work_path = work.shift
          begin
            if File.directory?(work_path)
              next if File.symlink?(work_path)
              work[0, 0] = filter!(work_path, Dir.entries(work_path), only_device)
            elsif File.file?(work_path)
              block[work_path]
            end
          rescue Errno::EACCES
            cfg.quiet or say "Warning: Skipping unreadable path: #{work_path}"
          end
        end until work.empty?
      end

      def filter!(parent_path, names, only_device=nil)
        names.map! do |name|
          next nil if name == "." || name == ".."

          path = "#{parent_path}/#{name}"
          next nil if cfg.skip_res.any? { |skip_re| skip_re.match?(path.b) }

          if File.directory?(path)
            next nil if ! cfg.dot_dirs && name.start_with?(".")
            next nil if ! cfg.src_dirs && cfg.src_dir_names.member?(name)
            next nil if only_device && File.stat(path).dev != only_device
          end

          path
        end
        names.compact!
        names.sort!
        names
      end

      def file_scanner_start
        @files_thread = Thread.new do
          seq = 0

          cfg.path_args.each do |path|
            only_device = cfg.keep_dev ? File.stat(path).dev : nil

            sub_files(path, only_device) do |file_path|
              files_queue.enq([seq += 1, file_path])
            end
          end

          files_queue.close
        end
      end

      def file_scanner_finish
        files_thread.join(3) or files_thread.kill
      end

      def cpu_count = ENV["CPUS"]&.to_i || TheC::Util.cpu_count

      def file_processor_pool_size = @file_processor_pool_size ||= [cpu_count, 8].min

      def file_processor_pool_start(&block)
        lines_threads_done = Queue.new

        @lines_threads = begin
          (1..file_processor_pool_size).map do |pool_member_id|
            Thread.new do
              block[pool_member_id, files_queue, lines_queue]
              lines_threads_done << pool_member_id
            end
          end
        end

        Thread.new do
          count = 0
          while lines_threads_done.deq
            count += 1
            break if count >= lines_threads.size
          end
          lines_threads_done.close
          lines_queue.close
        end
      end

      def file_processor_pool_finish
        if lines_threads.any? { |t| ! t.join(3) }
          lines_threads.each(&:kill)
        end
      end

      def display_lines
        next_seq = 1
        prev_seqs = {}
        while (seq_lines = lines_queue.deq)
          prev_seqs[seq_lines[0]] = seq_lines[1]
          while (lines = prev_seqs.delete(next_seq))
            lines.empty? or say lines.join, nolf: true
            next_seq += 1
          end
        end
      end
    end # Core

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
    end # Cli
  end # Ffg

  class RecursiveList
    def usage(msg=nil, exit_code=1)
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

    def run(args, io: $stdout)
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
        arg =~ /^[^-]/       && ok = 1 and paths << arg
        usage("Invalid option: #{arg.inspect}") if ok < 1
      end

      nodes = (paths.empty? ? ["."] : paths).map { |path| FileNode.new(path).to_a }.flatten

      if sort != 0
        if    by_sz    then nodes.sort! { |a, b| (sort * 2 * (b.size <=> a.size)) + (a.path <=> b.path) }
        elsif by_count then nodes.sort! { |a, b| (sort * 2 * (b.count <=> a.count)) + (a.path <=> b.path) }
        elsif by_name  then nodes.sort! { |a, b| sort * (a.path <=> b.path) }
        else  nodes.sort! { |a, b| (sort * 2 * (b.stat.mtime <=> a.stat.mtime)) + (a.path <=> b.path) }
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
          TheC::Util.commafy(node.size),
          TheC::Util.commafy(node.count),
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

  class Roots
    def usage(msg=nil, exit_code=1)
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

    def run(args, io: $stdout)
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
        nodes = FileNode.new(root) { |cfg| cfg.prune!(subdir) }
        full and $stderr << "{### Full tree:\n" + nodes.to_tree << "###}\n"
        name_roots = begin
          nodes.to_a.select do |node|
            File.basename(node.path) == subdir
          end.tap do |list|
            list.sort!
          end.map do |node|
            File.dirname(File.expand_path(node.path)) + "\n"
          end.join
        end
        ok = false if name_roots.empty?
        io << name_roots
      end
      ok
    end
  end # Roots

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
  end # SetPath

  ## Support comparing dir tree contents by recursively computing SHA256 hashes
   # of all files below given dirs. Using multi CPUs is supported as an option,
   # but it doesn't speed things up unless the disk(s) being accessed are fast
   # and multiplex (e.g. not platter, USB, or network).
   #
  class RecursiveHash
    include Mixin::Output

    RECORD = Struct.new(:path, :idx, :file, :sha, :sz, :ts1, :ts2)

    attr :max_cpus, :paths, :sort

    ## Construct.
     # @param paths The files or dirs to process, as an Array of String, or a Hash
     #    with each key as a dir and its values as Array of names within that dir.
     # @param sort How to sort final results. Use :path, :hash, :size, or nil. If
     #    nil, the sort is selected automatically by a heuristic, which is to sort
     #    by path if a basename in `paths` is used twice, otherwise sort by hash.
     # @param max_cpus How many CPUs to use. Default 2 (or 1 if 1 CPU exists).
     #    If nil or < 1, then use the max possible value for the machine.
     #    Any value is maxed at the machine CPU count.
     #
    def initialize(paths, sort, max_cpus: 2)
      if Hash === paths
        @paths = paths.map { |dir, files| files.map { "#{dir}/#{_1}" } }.flatten
      elsif Array === paths
        @paths = paths
      else
        raise "Invalid type for paths: Array or Hash is valid: #{paths.class}"
      end
      @sort = sort
      if ! max_cpus || max_cpus < 1
        @max_cpus = TheC::Util.cpu_count
      else
        @max_cpus = [max_cpus, TheC::Util.cpu_count].min
      end
      @last_reline = 0
    end

    ## Do the work and display results. For multi CPUs the value may be adjusted
     # down if the total file count is less than the desired CPU count.
     #
    def run
      file_tups = find_file_tuples
      return if file_tups.empty?

      pool_sz = [max_cpus, file_tups.size].min

      ## This is the STANDARD BASIC PATTERN for a pool of parallel worker processes.
       #
       # Step 1: Instantiate a worker for each member of the pool, which will be
       #         assigned its own CPU, and possibly its own address space, and which
       #         asynchronously receives an input to process and sends an output
       #         result.
       #
      my_actors = actor_list(pool_sz)

      ## Step 2: Instantiate an asynchronous thread, which doesn't care about CPU
       #         allocation, in this code's address space, assigned to each member
       #         of the pool in Step 1, that waits for an async result from its
       #         pool worker, and puts that result on a thread safe queue in local
       #         memory.
       #
      resultq = Queue.new
      recvers = my_actors.map do |my_actor|
        Thread.new do
          while (result = my_actor.receive) # Nil means worker is done
            path, idx, file, sha, sz, ts1, ts2 = result
            resultq.enq(RECORD.new(path, idx, file, sha, sz, ts1, ts2))
          end
        end
      end

      ## Step 3: Instantiate an asynchronous thread, which doesn't care about CPU
       #         allocation, in this code's address space, which pulls results from
       #         the queue in Step 2, and adds them to a local var.
       #
      raw_recs = []
      get_recs = Thread.new do
        while (result = resultq.deq) # Nil means all Step 2 threads are done
          raw_recs << result
          progress(result.file, raw_recs.size, file_tups.size)
        end
      end

      ## Step 4: Distribute all the input values to be processed across the pool
       #         of workers in Step 1. This step is where the CPUs in the pool
       #         actually start doing work. This should be synchronous and block
       #         here until all the work items have been passed to the pool and
       #         are being processed.
       #
      progress(file_tups.first[2], 0, file_tups.size) # Update screen for user
      file_tups.each_with_index do |t, i|
        ## This is the simplest possible method: Next line will block until this
         # particular worker is free, which is bad if some other worker is free
         # at this moment. A good improvement would be to track which workers are
         # free, and select one of those. Another method would be to use an input
         # queue for each worker, so the equivalent of this line would never block.
         # But in that case we would still need other code to wait for all input
         # queues to be empty after this loop.
         #
        my_actors[i % pool_sz].send(t)
      end

      ## Step 5: Wait for all work to finish. The order here is strict:
       #         1. Tell each worker in the pool that no more work will be sent.
       #         2. Wait for every thread in Step 2 to be finished.
       #         3. Tell the async thread in Step 3 to finish.
       #         4. Wait for the async thread in Step 3 to finish.
       #         After this step, all results from all pool members should be in
       #         the local var in Step 3, in random order.
       #
      my_actors.each(&:finish) # Step 5.1
      recvers.each(&:join)     # Step 5.2
      resultq.close            # Step 5.3
      get_recs.join            # Step 5.4
      reline ""

      ## Step 6: Process the random results in the local var in Step 3, to put
       #         them in order and be returned/displayed.
       #
      rows = reconsolidate_records(raw_recs)
      final_sort!(rows)
      display(rows)
    end

    ## Simply display the results in the list of `RECORD` objects.
     #
    def display(rows)
      width = paths.map(&:size).max
      align = sort == :path ? "-" : ""
      rows.each do |r|
        m = r.sz / 1e6
        et = r.ts2 - r.ts1
        puts("%s %#{align}#{width}s (%4.2fMB/%3.1fs = %4.2fMB/s)" % [
          r.sha, r.path, m, et, m / et
        ])
      end
    end

    ## Given the data that is ready to display, ensure it is sorted properly.
     #
    def final_sort!(rows)
      ensure_sort(rows)
      if    sort == :hash then rows.sort! { |a, b| 2 * (a.sha <=> b.sha) + (a.path <=> b.path) }
      elsif sort == :size then rows.sort! { |a, b| 2 * (b.sz <=> a.sz) + (a.path <=> b.path) }
      elsif sort == :path
        rows.sort! do |a, b|
          a1, b1 = File.basename(a.path), File.basename(b.path)
          a2, b2 = File.dirname(a.path), File.dirname(b.path)
          2 * (a1 <=> b1) + (a2 <=> b2)
        end
      end
    end

    ## Make sure the sort style for displayed data is set to some rational value.
     #
    def ensure_sort(rows)
      return sort if sort
      bnames = rows.map { File.basename(File.expand_path(_1.path)) }
      @sort = bnames.to_set.size < bnames.size ? :path : :hash
    end

    ## Given random results from the worker pool, convert to rows that can be
     # displayed. In this case each file processed has its own record, so those
     # must be grouped back together into the original paths that the user specified.
     #
    def reconsolidate_records(raw_recs)
      shadig = Digest::SHA256.new
      by_path = raw_recs.group_by { _1.path }
      rows = []
      by_path.each do |path, raw_recs|
        shadig.reset
        sz = 0
        ts1, ts2 = Float::INFINITY, 0.0
        raw_recs.sort do |a, b|
          a.idx <=> b.idx
        end.each do
          shadig << _1.sha
          sz += _1.sz
          ts1 = _1.ts1 if _1.ts1 < ts1
          ts2 = _1.ts2 if _1.ts2 > ts2
        end
        rows << RECORD.new(path, 0, 0, shadig.hexdigest, sz, ts1, ts2)
      end
      rows
    end

    ## Build a list of independent asynchronous workers. In this case each worker
     # is a separate process, linked to this process by anonymous kernel pipes.
     #
    def actor_list(pool_sz)
      (0...pool_sz).map do
        TheC::Practor.new.start do |actor|
          shadig = Digest::SHA256.new
          csz = 2**18 # Use 256KiB buffer per actor
          chunk = "".b
          while (path, idx, file = actor.receive) # Nil means all work items have been sent
            shadig.reset
            sz = 0
            ts1 = Time.now.to_f
            File.open(file, "rb") do |io|
              while io.read(csz, chunk)
                sz += chunk.size
                shadig << chunk
              end
            end
            ts2 = Time.now.to_f
            actor.send([path, idx, file, shadig.hexdigest, sz, ts1, ts2])
          end
        end
      end
    end

    ## If any given user paths are dirs, recursively descend them and return their
     # file info here. Each result member is a 3-tuple of:
     # (orig_dir_path, index_within_dir_path, full_file_path)
     #
    def find_file_tuples
      files = []
      paths.each_with_index do |path, idx|
        stat = File.lstat(path)
        unless stat.directory? || stat.file?
          puterr("Skipping: Not a dir or file: #{path.inspect}")
          next
        end
        Find.find(path).select do
          File.lstat(_1).file?
        end.sort.each_with_index do |f, i|
          files << [path, i, f]
        end
      end
      files
    end

    ## Display progress info to terminal screen.
     #
    def progress(file, index, total) = reline "%s (%d/%d)..." % [file, index, total]

    ## Support rewriting the same terminal line for messages.
     #
    def reline(msg)
      c1, c2 = %W[\b \ ].map { _1 * @last_reline }
      @last_reline = (msg[/\n([^\n]*)\z/, 1] || msg).size
      $stderr << c1 << c2 << c1 << msg
    end
  end # RecursiveHash

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
  end # Cli

  module Shortcuts
    ## Add the DSL for definining shortcuts to the includer.
     #
    module Dsl
      module DslMethods
        attr_reader :added_names

        ## Add a new Ruby shortcut in a DSL style. See calls to this in `TheC::Shortcuts::Core`.
         #
        def add(name, description, body)
          raise "Arg 'body' must be a proc" if ! Proc === body
          name = name.to_sym
          @added_names ||= {}
          added_names[name] = [description.to_s]
          define_method(name, body)
        end
      end # DslMethods

      def self.included(includer)
        includer.extend DslMethods
      end
    end # Dsl

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
    end # Helpers

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
          c:   "~/code",
          b:   "~/code/bash",
          f:   "~/shop/FLR",
          gh:  "~/code/gh",
          pca: "~/code/work/PreciousCargoAlert",
          r:   "~/code/rb",
          s:   "~/shop",
          t:   "~/tmp",
          v:   "~/media/Video",
          uNN: "(cd up NN dir levels)"
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
        if args.any?
          names = args.dup
        else
          names = %w[
            dan@homer:.bashrc pi@homer:.bashrc root@homer:.bashrc
            dan@missybook:.bashrc root@missybook:.bashrc
            dvr@dvr:dan.bashrc root@dvr:.bashrc
          ]
        end

        names.map! { |n| n.include?(":") ? n : "#{n}:hig.bashrc" }
        width = names.map(&:size).max
        source = "#{ENV["HOME"].shellescape}/.bashrc"

        ok = true
        names.each do |name|
          label = "#{name} ".ljust(width + 4, ".")
          print "#{label} "

          if name[0] == "!"
            puts "skipped."
            next
          end

          ok2 = bash("scp -q #{source} #{name.shellescape}", :errs).ok?
          puts "OK." if ok2
          ok &= ok2
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
            result = "checkupdates #{args[1..-1].shelljoin} | #{c :m}"
          elsif args[0][0..1] == "-Q"
            result = "#{c :sudo, @upd, *args} | #{c :m}"
          end
        end
        result || c(:sudo, @upd, *args)
      end

      add :ua, "Run aura", ->(*args) do
        if args[0] == "c"
          args = %w[-Auyd --log-level=info]
        else
          args = args.map { _1 =~ /^-S(.*)/ ? "-A#{$~[1]}" : _1 }
        end
        "aura #{args.shelljoin}"
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
    end # Core
  end # Shortcuts
end # TheC

TheC::Cli.new(ARGV) if $0 == __FILE__
