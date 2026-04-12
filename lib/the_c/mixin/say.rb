# frozen_string_literal: true

module TheC
module Mixin
  ## Output text to a configured IO, with default `$stdout`.
   #
  module Say
    class << self
      attr_accessor :io
    end
    self.io = $stdout

    ## Join `strings` and output to current value of `Say.io`, iff it's set.
     #
    def say(*strings, nolf: false)
      return if ! Say.io
      Say.io << "++ #{self.class.name} #{$$}: " if ! nolf
      Say.io << strings.join
      Say.io << "\n" if ! nolf || strings.empty?
    end
  end
end
end
