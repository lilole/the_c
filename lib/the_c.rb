#!/usr/bin/env ruby
# frozen_string_literal: true
#
# Copyright 2024-2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

=begin
  Every shell shortcut macro/script/tool in Ruby code.

  Notes:
  - Requires Ruby 3.1+ and Linux.

  - This is meant to be used in a bashrc to work alongside a builtin "c" function. For an example see:
      https://github.com/lilole/the_c/blob/main/the_c.bashrc

  - This is designed as a local command service, by starting up in the background and communicating with clients by
    named pipes. This design is crazy fast but requires special care to handle I/O.

  - Shortcuts ALWAYS return a String value to evaluate in the caller shell. This value may be a bit of bash code that
    processes stdin and updates stdout, or a bash keyword like `true` or `false` to signal success or failure.

  - Shortcuts MUST accept exactly 1 param, to be an instance of TheC::ShortcutParams. This object has some helper
    methods and 4 attrs: args, stdin, stdout, stderr. The first attr is the list of args given on the command line, and
    the other 3 attrs are substitute IO objects for $stdin, $stdout, and $stderr set for each specific shortcut's run.

  - Shortcuts MUST NOT assume that $stdin, $stdout, or $stderr are valid IO objects; they MUST NOT be touched.

  - Remember that shortcuts may call other shortcuts, and may even be used in pipelines of shortcuts.

  - It's safe for shortcuts to raise error for any abrupt/abnormal end condition. The main handler code will catch all
    exceptions, including `SystemExit`, and display the `Error` object's message.

  - The shortcuts are defined in `lib/the_c/shortcuts/core.rb`, and the reusable helper methods are in
    `lib/the_c/shortcuts/helpers.rb`. Follow the patterns here to tweak for your own env. All available features and
    their usage should become self evident from the patterns.
=end

module TheC
  VERSION = "2.6.100"

  PID_S = $$.to_s.freeze
end

#{{package_bootstrapper}}
