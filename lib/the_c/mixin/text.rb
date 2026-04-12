# frozen_string_literal: true

module TheC
module Mixin
  module Text
    ## Convert a number to a String with thousands separators.
     #
    def commafy(n)
      s = String === n ? n.dup : n.to_s
      s.tap { |v| v.reverse!; v.gsub!(/(\d{3})(?=\d)(?!\d*\.)/, "\\1,"); v.reverse! }
    end
  end
end
end
