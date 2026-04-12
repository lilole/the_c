# frozen_string_literal: true

module TheC
module Mixin
  ## Generic methods to output text.
   #
  module Output
    ## Shorthand for outputting error/warning/info messages on `$stderr` in
     # user's console.
     #
    def puterr(*strings, nolf: false)
      $stderr << strings.join
      $stderr << "\n" if ! nolf || strings.empty?
    end
  end
end
end
