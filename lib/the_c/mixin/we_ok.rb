# frozen_string_literal: true

module TheC
module Mixin
  ## Track valid status.
   #
  module WeOk
    class AreWeOk
      def yes!(*args)
        if args.empty?
          if   Numeric === @ok then @ok += 1
          else @ok = true
          end
        elsif args.size == 1
          value = args[0]
          if   value == :now then @ok
          else @ok = value
          end
        else
          raise "Only 0 or 1 arg allowed"
        end

        self
      end

      def now = @ok

      def now?
        if   Numeric === @ok then @ok != 0
        else !! @ok
        end
      end
    end # AreWeOk

    ## Access current status object.
     #
    def are_we_ok = @are_we_ok ||= AreWeOk.new

    ## With an arg, set the current status, to either Numeric or boolean.
     # With no arg, increment current status if Numeric, or reset to true.
     #
    def ok!(*args) = are_we_ok.yes!(*args)

    ## Read current status. Numeric is ok if > 0.
     #
    def ok? = are_we_ok.now?
  end
end
end
