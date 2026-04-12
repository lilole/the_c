# frozen_string_literal: true

module Extensions
  def self.apply
    ::String.prepend TheC::Ext::String
  end
end
