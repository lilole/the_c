#!/usr/bin/env ruby
# frozen_string_literal: true
#
# Copyright 2024-2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

## Every shell shortcut macro/script/tool in a single block of Ruby code.
 #
 # Notes:
 # - Requires Ruby 3.1+ and Linux.
 # - This is meant to be used in a bashrc to work alongside a custom "c"
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

module TheC
  VERSION = "2.6.40"
end

#{{package_bootstrapper}}
