# frozen_string_literal: true

module TheC
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
  end
end
