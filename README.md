# the_c

_Every shell shortcut macro/script/tool in a single block of Ruby code._

## What is this?

This is a sidecar process that runs a collection of bash shortcuts that I have collected over the past 25+ years of professional coding. It's not the full collection. That would be ridiculously long. Old ones, for automation of tools like CVS, svn, and previous tools for specific jobs I had, are cleaned out of this.

The more unique thing about this particular collection is that it's all in Ruby. This was done as a little mental exercise, to see if I could make the Ruby code for the equivalent Bash code look "better" or run "better" without adding too many extra lines.

I believe it has been successful for me. I have learned a bit, and gotten some new reusable Ruby code out of it, and ~~will soon be~~ am now running this for my daily driving. In most cases, the shortcuts ported over to Ruby have added functionality, like actual option checking, and online help, and decent error handling.

The Ruby code is embedded in the rc file because I like everything in a single file, because it lets me ship one file around to many servers and always have the shortcuts I like. Of course in reality this is not required. You could use the pattern here and keep the Ruby code in its own file, or even create your own shell sidecar in any lang.

## But, what _is_ it?

Being a _sidecar process_ means that the shortcut code runs in its own background process alongside the shell (in my case bash). If your shell is not bash it can still work, as long as your shell is fairly Bourne-shell compatible.

There are a few things in this code that could be made into useful high-level patterns in any language. Such as:

- Integrating a command service with any bash-like shell.
- Using named pipes for easy line-based text IPC.
- Using anonymous pipes for binary IPC.
- Starting up thread pools and properly waiting for them to finish, using only thread-safe queues.
- Processing text with a sliding context window.
- Capturing a process's stdio into variables, with any lang that has closures.
- Using fork for the actor pattern.

Specific to Ruby, some notable reusable things here are:

- Adding a custom DSL to a class with plain old Ruby.
- Generating Ruby with Ruby in a background process to execute any block of Ruby in a foreground process.
- Making a souped-up `%x{}` operator that runs shell script code and returns an object with captured output and status.
- Making an actor that is easier to use than Ractor, and hides no details, in under 50 lines of code.
- A reusable method for doing single-key prompted inputs with a one-liner, using `io/console`.
- Decent CLI argument parsing with full control, without any extra libs.

If you want to make your own shell sidecar for a command service, using some other lang, then you could do that while reusing the code that integrates the bashrc with the sidecar.

## Notes

These are taken directly from the `the_c.rb` file.

```text
- Requires Ruby 3.1+ and Linux.
- This is meant to be pasted into a bashrc to work alongside a custom "c"
  function. For an example see:
    https://github.com/lilole/the_c/blob/main/bashrc.example
- Defined Ruby shortcuts MUST return one of these standard types:
  - `nil`.......Reports a nop to the caller.
  - `String`....A bit of bash code to evaluate in the caller's context.
  - `Integer`...Return the value as an exit code to the caller.
  - truthy......Reports a success to the caller.
  - falsey......Reports a failure to the caller.
- This is designed to be a local command service, by starting up in the
  background and communicating with clients by named pipes. This design is
  crazy fast but also puts some constraints on the code for I/O.
- Shortcut worker methods MUST accept an `io` param if they need to output
  results to the tty. The `io` param may simply turn out to be `$stdout`,
  but the worker method MUST NOT assume it writes to `$stdout`.
- In general shortcuts should only use `$stderr` for info messages to user.
- In general shortcuts should try first to return a String that would be
  evaluated as bash code in the caller's context. This is the best way to
  ensure that shortcuts can call other shortcuts, and can even be used in
  pipelines of shortcuts.
- It's safe for shortcuts to raise error for any abrupt/abnormal end
  condition. The main handler code here will catch all exceptions,
  including `SystemExit`, and display the `Error` object's message.
- The `:m` shortcut is a special case, because the `page` helper method
  uses it internally for cases where large amounts of data may need to be
  viewed.
- Follow the patterns here to tweak for your own env. All available
  features and their usage should become self evident from the patterns.
```

## Future plans

- More docs/comments.<br>I have added a lot of docs already, with foldable comment blocks, but more need to be added.
- More testing.<br>There are _probably_ bugs here. Notably I have not really tested this bashrc with active Ruby projects on different Ruby versions. This code would break on any Ruby before 3.1.<br>The workround would probably be to find the "real" Ruby version, either system or rbenv/rvm/whatever, from within the home dir at .bashrc load time, and cache its path in an env var.
- More factoring.<br>There is some code in different Modules that can be reused more, across multiple Modules.
- More readability.<br>Originally, I was afraid that the Ruby needed to be as "compact" as possible, because I was loading the entire Ruby file for each `c` call. Now the Ruby runs as a background service, so the Ruby bits can grow and grow. But there are remnants of the old "wider" not "taller" compacted Ruby. I still want to air out the Ruby code and make it more readable and idiomatic (in that order). This would also include keeping all the comments/docs.<br>Overall, this code should already be easy to trace with your eyeballs, and will continually become easier over time.
- Tiny performance enhancements.

## Contributing

Well sure, if you like. I put this in the wild just in case it helps other Ruby devs to customize their CLI, but additions or improvements are always welcome. You can click the Issues tab above and we can go from there.

Cheers.
