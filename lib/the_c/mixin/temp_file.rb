# frozen_string_literal: true

module TheC
module Mixin
  module TempFile
    include Info

    ## Start a single thread that cleans specific glob patterns that are older
     # than a specific number of secs, checking once per minute.
     # Use `add_to_clean` to add globs and ttls to be cleaned here.
     #
    if $TheC__Mixin__TempFile_state.nil?
      ->(state) do
        state[:to_clean_tups] = []
        state[:clean_thread] = begin
          Thread.new do
            loop do
              state[:to_clean_tups].each do |(ttl_secs, glob_pattern)|
                cutoff = Time.now - ttl_secs
                Dir.glob(glob_pattern, File::FNM_DOTMATCH).each do |file|
                  begin
                    File.delete(file) if File.mtime(file) <= cutoff
                  rescue
                    ## If we get here the `mtime` or `delete` failed, so it's
                     # already gone. This is fairly common because other processes
                     # will have this same thread with the same globs.
                    nil
                  end
                end
              end
              sleep 60
            end
          end
        end
      end.call($TheC__Mixin__TempFile_state = {})
    end

    def self.add_to_clean(ttl_secs, glob_pattern)
      pairs = $TheC__Mixin__TempFile_state[:to_clean_tups]
      new_pair = [ttl_secs, glob_pattern]
      pairs << new_pair if ! pairs.member?(new_pair)
    end

    ## Create a unique temp file with optional given body, and return an object
     # with `write`, `path`, and `io` methods. Use the `write` method to add to
     # the file. When done writing, use the `path` method to close and return
     # the file's path. Use `io` for direct access to the open File object.
     #
     # A single thread in this process will monitor created tmp files and clean
     # any that were touched 30+ mins ago, each minute, from any process.
     #
    def make_temp_file(body=nil)
      cache = @make_temp_file_cache ||= begin
        result = {}
        result[:seq] = 0
        result[:template] = "#{fast_tmp_dir}/#{my_classname}-#{$$}-{{SEQ}}.tmp"
        result[:class] = Class.new do
          attr :io
          def initialize(path) = (@io = File.open(path, "w"); File.chmod(0600, path); io.sync = true)
          def write(data)      = io.write(data)
          def path             = (io.close; io.path)
        end
        TempFile.add_to_clean(30 * 60, "#{fast_tmp_dir}/#{my_classname}-*.tmp")
        result
      end

      begin
        seq = (cache[:seq] += 1)
        tmpfile = cache[:template].sub("{{SEQ}}", seq.to_s)
      end while File.exist?(tmpfile)

      tmpobj = cache[:class].new(tmpfile)
      tmpobj.write(body) if body
      tmpobj
    end
  end
end
end
