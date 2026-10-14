# frozen_string_literal: true
#
# Copyright 2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

module TheC
  class ShortcutParams
    attr_rw %i[stderr stdin stdout]

    def set!(*name_value_seq)
      name_value_seq.each_slice(2) { |name, value| send("#{name}=", value) }
      stdout.sync = true if stdout
      self
    end

    def gets(*args, **kargs) = stdin.gets(*args, **kargs)

    def puts(*objects, nl: :default, del: "\n", out: stdout)
      text = objects.map { |o|
        case o
        when String    then o
        when Array     then o.inspect
        when Exception then o.full_message
        else o.to_s
        end
      }.join(del)
      out << text
      out << "\n" if nl == :always || nl == :default && (text.empty? || text[-1] != "\n")
      nil
    end

    def pute(*objects) = puts(*objects, del: "", out: stderr)
  end
end
