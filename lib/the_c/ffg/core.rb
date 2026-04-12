# frozen_string_literal: true

module TheC
module Ffg
  class Core
    include TheC::Mixin::Say

    attr_reader :cfg, :files_queue, :files_thread, :lines_queue, :lines_threads

    def initialize(cfg)
      @cfg = cfg
      @files_queue = Queue.new
      @lines_queue = Queue.new
    end

    def run
      if cfg.fg
        Grep.new(self, cfg).run
      else
        Find.new(self, cfg).run
      end
    end

    def standard_client_run(&client_file_processor)
      file_scanner_start
      begin
        file_processor_pool_start do |pool_member_id, files_queue, lines_queue|
          client_file_processor[pool_member_id, files_queue, lines_queue]
        end
        begin
          display_lines
        ensure
          file_processor_pool_finish
        end
      ensure
        file_scanner_finish
      end
    end

    def sub_files(dir_or_file, only_device=nil, &block)
      work = [dir_or_file] # Depth-first stack, no recursion needed baby
      begin
        work_path = work.shift
        begin
          if File.directory?(work_path)
            next if File.symlink?(work_path)
            work[0, 0] = filter!(work_path, Dir.entries(work_path), only_device)
          elsif File.file?(work_path)
            block[work_path]
          end
        rescue Errno::EACCES
          cfg.quiet or say "Warning: Skipping unreadable path: #{work_path}"
        end
      end until work.empty?
    end

    def filter!(parent_path, names, only_device=nil)
      names.map! do |name|
        next nil if name == "." || name == ".."

        path = "#{parent_path}/#{name}"
        next nil if cfg.skip_res.any? { |skip_re| skip_re.match?(path.b) }

        if File.directory?(path)
          next nil if ! cfg.dot_dirs && name.start_with?(".")
          next nil if ! cfg.src_dirs && cfg.src_dir_names.member?(name)
          next nil if only_device && File.stat(path).dev != only_device
        end

        path
      end
      names.compact!
      names.sort!
      names
    end

    def file_scanner_start
      @files_thread = Thread.new do
        seq = 0

        cfg.path_args.each do |path|
          only_device = cfg.keep_dev ? File.stat(path).dev : nil

          sub_files(path, only_device) do |file_path|
            files_queue.enq([seq += 1, file_path])
          end
        end

        files_queue.close
      end
    end

    def file_scanner_finish
      files_thread.join(3) or files_thread.kill
    end

    def cpu_count = ENV["CPUS"]&.to_i || TheC::Util.cpu_count

    def file_processor_pool_size = @file_processor_pool_size ||= [cpu_count, 8].min

    def file_processor_pool_start(&block)
      lines_threads_done = Queue.new

      @lines_threads = begin
        (1..file_processor_pool_size).map do |pool_member_id|
          Thread.new do
            block[pool_member_id, files_queue, lines_queue]
            lines_threads_done << pool_member_id
          end
        end
      end

      Thread.new do
        count = 0
        while lines_threads_done.deq
          count += 1
          break if count >= lines_threads.size
        end
        lines_threads_done.close
        lines_queue.close
      end
    end

    def file_processor_pool_finish
      if lines_threads.any? { |t| ! t.join(3) }
        lines_threads.each(&:kill)
      end
    end

    def display_lines
      next_seq = 1
      prev_seqs = {}
      while (seq_lines = lines_queue.deq)
        prev_seqs[seq_lines[0]] = seq_lines[1]
        while (lines = prev_seqs.delete(next_seq))
          lines.empty? or say lines.join, nolf: true
          next_seq += 1
        end
      end
    end
  end
end
end
