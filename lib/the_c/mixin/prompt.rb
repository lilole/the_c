# frozen_string_literal: true

module TheC
module Mixin
  module Prompt
    include Output

    ## Prompt user for confirmation before continuing.
     # The `opts` may be any string of typable characters, with a single
     # uppercase to be the default if user presses Enter.
     # If the choice is "q", then exit immediately.
     # If the choice is "y" or "n", then return Boolean.
     # Any other choice returns the character.
     #
    def ask_continue(prompt="Continue?", opts="Ynq")
      def_reply = opts.gsub(/[^A-Z]+/, "")
      raise "Only 1 uppercase is allowed: #{opts.inspect}" if def_reply.size > 1
      puterr
      begin
        puterr "#{prompt} [#{opts}] ", nolf: true
        reply = $stdin.getch(intr: true).chomp
        reply = def_reply if reply.empty? && ! def_reply.empty?
        lreply = reply.downcase
        puterr lreply
      end until lreply =~ /^[#{opts.downcase}]$/
      puterr
      exit if lreply == "q"
      %w[y n].member?(lreply) ? lreply == "y" : reply
    end
  end
end
end
