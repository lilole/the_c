# frozen_string_literal: true

## Automatically setup for autoloading files that define constants such as
 # modules or classes. This is meant to be a single file, to be copied to each
 # project that needs it.
 #
module AutAut
  VERSION = "1.6.41"

  ## The contents of each given directory tree are directly mapped to constant
   # paths, based on the typical naming convention, and file subpaths are mapped
   # to constant names that are autoloaded.
   #
   # Here is an example to show the mapping pattern. Given this file structure:
   #
   #   ...
   #   └─ my_project_root/
   #      └─ lib/
   #         ├─ my_cool_project/
   #         │  ├─ mixins/
   #         │  │  ├─ cool.rb
   #         │  │  └─ neat.rb
   #         │  ├─ cli.rb
   #         │  ├─ core.rb
   #         │  └─ options.rb
   #         └─ my_cool_project.rb
   #
   # If you run:
   #
   #   AutAut.setup "/path/to/my_project_root/lib"
   #
   # Then you would have these constants automatically accessible from your
   # code, which autoload the files above:
   #
   #   MyCoolProject::Mixins::Cool
   #   MyCoolProject::Mixins::Neat
   #   MyCoolProject::Cli
   #   MyCoolProject::Core
   #   MyCoolProject::Options
   #   MyCoolProject
   #
   # For long-term organizational sanity, adhere to these 2 rules:
   # (a) Branches in the tree should always be modules, and
   # (b) Leaves in the tree should be modules or classes.
   # That is, *.rb files should only define constants that are modules or
   # classes, and any *.rb file whose base matches a subdir name SHOULD define a
   # module, not a class, for that name.
   #
   # In the example above, all *.rb files could define classes or modules for
   # their names, EXCEPT my_cool_project.rb, which SHOULD NOT define a class
   # called "MyCoolProject" because it has a subdir (branch) with matching
   # basename.
   #
  def self.setup(*dirs)
    for dir in dirs.flatten.map! { |d| File.realpath(d) }
      for rb_path in Dir.glob("**/*.rb", base: dir).sort!
        modnames  = rb_path.split("/").map! { |filename| to_modname(filename) }
        constname = modnames.slice!(-1)
        modpath   = to_modpath(modnames)
        eval "%s.autoload(:%s, %s)" % [modpath, constname, "#{dir}/#{rb_path}".inspect]
      end
    end
  end

  UNSAFE_RE = /\W+/ # Add security for filenames by hackers

  ## Convert a filename to its equivalent Module/Class name. This is the most
   # basic snake-case to camel-case conversion: Each word delimited by `_` has
   # its first char upcased, and remaining chars downcased. Non-word chars are
   # ignored.
   #
  def self.to_modname(filename)
    File.basename(filename, ".*")
      .split("_")
      .map! { |word|
        word.gsub!(UNSAFE_RE, "")
        word.capitalize!
      }.join
  end

  ## Ensure each branch in the given module names is created, and return the
   # absolute module path from all the names, or `::Object` if no names passed.
   #
  def self.to_modpath(modnames)
    modnames.reduce(nil) { |fullpath, nextname|
      "#{fullpath}::#{nextname}".tap { |newpath|
        eval "#{newpath} = ::Module.new" if ! ::Object.const_defined?(newpath)
      }
    } || "::Object"
  end
end
