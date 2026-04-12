# frozen_string_literal: true

module TheC
module Ffg
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
  end
end
end
