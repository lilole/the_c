# frozen_string_literal: true
#
# Copyright 2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

module TheC
  class ShortcutParams
    @@uid = 0

    attr_rw %i[args stderr stdin stdout]

    def set!(*name_value_seq)
      name_value_seq.each_slice(2) { |name, value| send("#{name}=", value) }
      stdout.sync = true if stdout
      self
    end

    def tmpfile(&gen_block)
      tmp = "/tmp/#{File.basename($0)}_#{$$}_#{next_uid}.tmp"
      File.open(tmp, "wt") { |io| gen_block[io] }
      Thread.new { sleep 60; File.unlink(tmp) }
      tmp
    end

    def next_uid = @@uid += 1

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
