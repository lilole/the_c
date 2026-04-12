# frozen_string_literal: true

module TheC
module Mixin
  ## Generic utility methods which may be included in any class.
   # NONE of these methods read or write any context from their includers.
   #
  module Util
    include Bash
    include Info
    include Output
    include Prompt
    include TempFile
    include Text
  end
end
end
