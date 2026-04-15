# frozen_string_literal: true

require "stringio"

module TheC
module Mixin
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
  end
end
end
