# frozen_string_literal: true

module TheC
module Shortcuts
  ## Add the DSL for definining shortcuts to the includer.
   #
  module Dsl
    def self.included(includer)
      includer.extend DslMethods
    end

    module DslMethods
      attr_reader :added_names

      ## Add a new Ruby shortcut in a DSL style. See calls to this in `TheC::Shortcuts::Core`.
       #
      def add(name, description, body)
        raise "Arg 'body' must be a proc" if ! Proc === body
        name = name.to_sym
        @added_names ||= {}
        added_names[name] = [description.to_s]
        define_method(name, body)
      end
    end # DslMethods
  end
end
end
