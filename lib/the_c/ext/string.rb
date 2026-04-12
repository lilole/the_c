# frozen_string_literal: true

module TheC
module Ext
  module String
    ## Allow `some_string[/(foo)(bar).../, 1, 2, ...]` to return an array of all
     # the captures. All array elements may be `nil` if there is no match.
     #
    def [](*args)
      if Regexp === args[0] && args.size > 2
        rem = args[0].match(self)
        args[1..-1].map { |idx| rem&.[](idx) }
      else
        super
      end
    end
  end
end
end
