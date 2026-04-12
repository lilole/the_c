# frozen_string_literal: true

module TheC
module Ffg
  class Grep
    include TheC::Mixin::Say

    attr_reader :cfg, :core, :my_actors

    def initialize(core, cfg)
      @core = core
      @cfg = cfg
    end

    def run
      # Since Practor forks, we instantiate all of them first to try to keep it simple
      start_actors

      core.standard_client_run do |pool_member_id, files_queue, lines_queue|
        actor_idx = pool_member_id - 1
        my_actor = my_actors[actor_idx]
        while (seq_path = files_queue.deq)
          my_actor.send(seq_path[1])
          lines = my_actor.receive
          lines_queue.enq([seq_path[0], lines])
        end
        my_actor.finish
      end
    end

    def start_actors
      @my_actors = begin
        (1..core.file_processor_pool_size).map do
          TheC::Practor.new.start do |actor|
            loop do
              path = actor.receive or break
              actor.send(grep_file(path))
            end
          end
        end
      end
    end

    def grep_file(file_path)
      path = file_path.b # Needed in rescue block
      result = []
      File.open(file_path, "rt") do |f|
        ctxt_val = (cfg.ctxt > 0) ? cfg.ctxt : nil
        if ctxt_val # TODO: Classify logic for this
          ctxt_buf = []
          last_match_num = last_result_num = nil
        end

        num = 0
        while (num += 1; line = f.gets)
          matched = cfg.re_arg.match?(line.b.chomp)

          if cfg.path_only
            if matched
              result << "#{path}\n"
              break
            end
            next
          end

          next unless ctxt_val || matched

          line = "%s:%03d: %s" % [path, num, line]
          if ctxt_val
            if matched
              # Output ctxt lines window and matched line
              result << "...\n" if last_result_num && num - 1 - ctxt_val > last_result_num
              result.concat(ctxt_buf) << line
              ctxt_buf.clear
              last_match_num = last_result_num = num
            elsif last_match_num && num - last_match_num <= ctxt_val
              # Output ctxt lines after last matched line
              result << line
              last_result_num = num
            else
              # Fill ctxt lines sliding window
              ctxt_buf.size < ctxt_val or ctxt_buf.shift(1 + ctxt_buf.size - ctxt_val)
              ctxt_buf << line
            end
          else # Matched, no extra context lines
            result << line
          end
        end
      end
      result
    rescue Errno::EACCES
      cfg.quiet or say "Warning: Skipping unreadable file: #{path}"
      []
    end
  end
end
end
