# frozen_string_literal: true

module TheC
module Ffg
  class Find
    attr_reader :cfg, :core

    def initialize(core, cfg)
      @core = core
      @cfg = cfg
    end

    def run
      core.standard_client_run do |_pool_member_id, files_queue, lines_queue|
        empty = [].freeze

        while (seq_path = files_queue.deq)
          path = seq_path[1].b

          if cfg.re_arg.match?(path)
            lines = ["#{path}\n"]
          else
            lines = empty
          end

          lines_queue.enq([seq_path[0], lines])
        end
      end
    end
  end
end
end
