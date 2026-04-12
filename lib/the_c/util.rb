# frozen_string_literal: true

module TheC
  ## Access to `TheC::Mixin::Util` methods without including.
   #
  module Util
    extend TheC::Mixin::Util
  end
end
