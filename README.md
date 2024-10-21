# the_c

_Every shell shortcut macro/script/tool in a single block of Ruby code._

## Overview

This is a collection of bash shortcuts that I have collected over the past 25+ years of professional coding. It's not the full collection. That would be ridiculously long. Old ones, for automation of tools like CVS, svn, and other specific job tools, are cleaned out of this.

The more unique thing about this particular collection is that it's all in Ruby. This was done as a little mental exercise, to see if I could make the Ruby code for the equivalent Bash code look "better" or run "better" without adding many extra lines.

I believe it has been successful for me. I have learned a bit, and gotten some new reusable Ruby code out of it, and will soon be running this for my daily driving. There are just a few shortcuts that have not yet been ported over from the current Bash code.

## Notes

- Requires Ruby 3.1+.
- This is meant to be pasted into a bashrc as the body of a "c" function.<br>
  For example:
    ```shell
    c() {
      if [[ ! ${THE_C_BODY:0:1} ]]; then
        THE_C_BODY=$(cat << '____END'
          # ...this file...
    ____END
        )
      fi
      eval "$(ruby -e "$THE_C_BODY" -- "$@")"
    }
    ```
- Defined Ruby shortcuts MUST return one of these standard types:
    ```text
    `nil`......Reports a nop to the caller.
    `String`...A bit of bash code to be evaluated in the caller's context.
    truthy.....Reports a success to the caller.
    falsey.....Reports a failure to the caller.
    ```
- Shortcut worker methods MUST accept an `io` param if they need to output
  results to the console. The `io` param may simply turn out to be
  $stdout, but the worker method MUST NOT assume it writes to $stdout.
- In general shortcuts should only use $stderr for info messages to user.
- In general shortcuts should try first to return a String that would be
  evaluated as bash code in the caller's context. This is the best way to
  ensure that shortcuts can call other shortcuts, and can even be used in
  pipelines of shortcuts.
- It's safe for shortcuts to raise error for any abrupt/abnormal end
  condition. The main handler code here will catch all exceptions,
  including SystemExit, and display the Error object's message.
- The `:m` shortcut is a special case, because the `page` helper method
  uses it internally for cases where large amounts of data may need to be
  viewed.
- Follow the patterns here to tweak for your own env. All available
  features and their usage should become self evident from the patterns.
- The Ruby code here is designed to be "wide" not "tall", to keep the
  line count lower. This means you'll see a lot of code here that is not
  idiomatic Ruby, but the code should still be readable and easy to trace
  with your eyeballs.

## Contributing

Well sure, if you like. I put this in the wild just in case it helps other Ruby devs to customize their CLI, but additions or improvements are always welcome. You can click the Issues tab above and we can go from there. Cheers.
