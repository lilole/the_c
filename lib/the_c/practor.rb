# frozen_string_literal: true

module TheC
  ## A rudimentary Ractor based on anonymous pipes.
   #
  class Practor
    attr_reader :actor_pid, :forked, :from_main_r, :from_main_w, :to_main_r, :to_main_w

    def initialize
      @from_main_r, @from_main_w = IO.pipe("ASCII-8BIT:ASCII-8BIT")
      @to_main_r,   @to_main_w   = IO.pipe("ASCII-8BIT:ASCII-8BIT")
    end

    def start(&block)
      @actor_pid = fork do
        @forked = true
        cap_unused_ends
        block[self]
      end
      Process.detach(actor_pid)
      cap_unused_ends
      self
    end

    def cap_unused_ends
      if forked
        from_main_w.close
        to_main_r.close
      else
        from_main_r.close
        to_main_w.close
      end
    end

    def send(object)
      pipe = forked ? to_main_w : from_main_w
      Marshal.dump(object, pipe)
      pipe.flush
      self
    end

    def receive
      pipe = forked ? from_main_r : to_main_r
      Marshal.load(pipe)
    rescue EOFError
      nil
    end

    def finish
      from_main_w.close
    end
  end
end
