# frozen_string_literal: true

module TheC
module Shortcuts
  ## Add the DSL for definining shortcuts to the includer.
   #
  module Dsl
    AddedNames = {}

    def self.included(includer)
      includer.extend DslMethods
    end

    module DslMethods
      ## Add a new Ruby shortcut in a DSL style. See calls to this in `TheC::Shortcuts::Core`.
       #
      def add(name, description, body1=nil, &body2)
        body = body1 || body2
        raise "Must pass a body as param or block" if body.nil?
        raise "Body must be a proc" unless Proc === body
        force_param = body.lambda?
        raise "Body must take exactly 1 param" if force_param && body.arity != 1 || body.arity > 1
        name = Symbol === name ? name : name.to_s.to_sym
        cache = Dsl::AddedNames
        raise "Shortcut already defined: #{name}: #{cache[name][0]}" if cache.key?(name)
        cache[name] = [description.to_s] # Use Array to store other details if needed
        define_method(name, body)
      end
    end # DslMethods
  end
end
end
