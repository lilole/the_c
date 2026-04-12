# frozen_string_literal: true

module TheC
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
  end
end
