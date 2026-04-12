# frozen_string_literal: true

module TheC
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
  end
end
