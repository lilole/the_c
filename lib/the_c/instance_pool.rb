# frozen_string_literal: true
#
# Copyright 2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

class Mutex
  if ! method_defined?(:synch_nest)
    def synch_nest(&block)
      lock_here = ! owned?
      lock if lock_here
      begin
        block[]
      ensure
        unlock if lock_here
      end
    end
  end
end

module TheC
  class InstancePool
    attr_ro   :max_size, :min_size, :user_class
    attr_priv :in_use, :lock, :objects

    def initialize(user_class, min_size: 5, max_size: 1000)
      self.user_class = user_class
      self.min_size   = min_size
      self.max_size   = max_size
      self.in_use  = Set[]
      self.lock    = Mutex.new
      self.objects = Array.new(min_size) { user_class.new }
    end

    def avail?(idx) = lock.synch_nest { ! in_use.member?(idx) }
    def avail!(idx) = lock.synch_nest { in_use.delete(idx); self }

    def in_use?(idx) = lock.synch_nest { in_use.member?(idx) }
    def in_use!(idx) = lock.synch_nest { in_use << idx; self }

    def use(&block)
      idx = nil
      lock.synch_nest {
        idx = 0; idx += 1 until avail?(idx)
        if idx >= objects.size
          raise "Max size exceeded" if objects.size >= max_size
          objects[idx] = user_class.new
        end
        in_use!(idx)
      }
      begin
        block[objects[idx]]
      ensure
        release(idx)
      end
    end

    def release(index)
      lock.synch_nest {
        avail!(index)
        if objects.size > min_size && in_use.size <= min_size
          objects.each_key { |idx|
            objects.delete(idx) if avail?(idx)
            break if objects.size <= min_size
          }
        end
      }
    end
  end
end
