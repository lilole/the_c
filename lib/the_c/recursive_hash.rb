# frozen_string_literal: true

module TheC
  ## Support comparing dir tree contents by recursively computing SHA256 hashes
   # of all files below given dirs. Using multi CPUs is supported as an option,
   # but it doesn't speed things up unless the disk(s) being accessed are fast
   # and multiplex (e.g. not platter, USB, or network).
   #
  class RecursiveHash
    include Mixin::Output

    RECORD = Struct.new(:path, :idx, :file, :sha, :sz, :ts1, :ts2)

    attr :max_cpus, :paths, :sort

    ## Construct.
     # @param paths The files or dirs to process, as an Array of String, or a Hash
     #    with each key as a dir and its values as Array of names within that dir.
     # @param sort How to sort final results. Use :path, :hash, :size, or nil. If
     #    nil, the sort is selected automatically by a heuristic, which is to sort
     #    by path if a basename in `paths` is used twice, otherwise sort by hash.
     # @param max_cpus How many CPUs to use. Default 2 (or 1 if 1 CPU exists).
     #    If nil or < 1, then use the max possible value for the machine.
     #    Any value is maxed at the machine CPU count.
     #
    def initialize(paths, sort, max_cpus: 2)
      if Hash === paths
        @paths = paths.map { |dir, files| files.map { "#{dir}/#{_1}" } }.flatten
      elsif Array === paths
        @paths = paths
      else
        raise "Invalid type for paths: Array or Hash is valid: #{paths.class}"
      end
      @sort = sort
      if ! max_cpus || max_cpus < 1
        @max_cpus = TheC::Util.cpu_count
      else
        @max_cpus = [max_cpus, TheC::Util.cpu_count].min
      end
      @last_reline = 0
    end

    ## Do the work and display results. For multi CPUs the value may be adjusted
     # down if the total file count is less than the desired CPU count.
     #
    def run
      file_tups = find_file_tuples
      return if file_tups.empty?

      pool_sz = [max_cpus, file_tups.size].min

      ## This is the STANDARD BASIC PATTERN for a pool of parallel worker processes.
       #
       # Step 1: Instantiate a worker for each member of the pool, which will be
       #         assigned its own CPU, and possibly its own address space, and which
       #         asynchronously receives an input to process and sends an output
       #         result.
       #
      my_actors = actor_list(pool_sz)

      ## Step 2: Instantiate an asynchronous thread, which doesn't care about CPU
       #         allocation, in this code's address space, assigned to each member
       #         of the pool in Step 1, that waits for an async result from its
       #         pool worker, and puts that result on a thread safe queue in local
       #         memory.
       #
      resultq = Queue.new
      recvers = my_actors.map do |my_actor|
        Thread.new do
          while (result = my_actor.receive) # Nil means worker is done
            path, idx, file, sha, sz, ts1, ts2 = result
            resultq.enq(RECORD.new(path, idx, file, sha, sz, ts1, ts2))
          end
        end
      end

      ## Step 3: Instantiate an asynchronous thread, which doesn't care about CPU
       #         allocation, in this code's address space, which pulls results from
       #         the queue in Step 2, and adds them to a local var.
       #
      raw_recs = []
      get_recs = Thread.new do
        while (result = resultq.deq) # Nil means all Step 2 threads are done
          raw_recs << result
          progress(result.file, raw_recs.size, file_tups.size)
        end
      end

      ## Step 4: Distribute all the input values to be processed across the pool
       #         of workers in Step 1. This step is where the CPUs in the pool
       #         actually start doing work. This should be synchronous and block
       #         here until all the work items have been passed to the pool and
       #         are being processed.
       #
      progress(file_tups.first[2], 0, file_tups.size) # Update screen for user
      file_tups.each_with_index do |t, i|
        ## This is the simplest possible method: Next line will block until this
         # particular worker is free, which is bad if some other worker is free
         # at this moment. A good improvement would be to track which workers are
         # free, and select one of those. Another method would be to use an input
         # queue for each worker, so the equivalent of this line would never block.
         # But in that case we would still need other code to wait for all input
         # queues to be empty after this loop.
         #
        my_actors[i % pool_sz].send(t)
      end

      ## Step 5: Wait for all work to finish. The order here is strict:
       #         1. Tell each worker in the pool that no more work will be sent.
       #         2. Wait for every thread in Step 2 to be finished.
       #         3. Tell the async thread in Step 3 to finish.
       #         4. Wait for the async thread in Step 3 to finish.
       #         After this step, all results from all pool members should be in
       #         the local var in Step 3, in random order.
       #
      my_actors.each(&:finish) # Step 5.1
      recvers.each(&:join)     # Step 5.2
      resultq.close            # Step 5.3
      get_recs.join            # Step 5.4
      reline ""

      ## Step 6: Process the random results in the local var in Step 3, to put
       #         them in order and be returned/displayed.
       #
      rows = reconsolidate_records(raw_recs)
      final_sort!(rows)
      display(rows)
    end

    ## Simply display the results in the list of `RECORD` objects.
     #
    def display(rows)
      width = paths.map(&:size).max
      align = sort == :path ? "-" : ""
      rows.each do |r|
        m = r.sz / 1e6
        et = r.ts2 - r.ts1
        puts("%s %#{align}#{width}s (%4.2fMB/%3.1fs = %4.2fMB/s)" % [
          r.sha, r.path, m, et, m / et
        ])
      end
    end

    ## Given the data that is ready to display, ensure it is sorted properly.
     #
    def final_sort!(rows)
      ensure_sort(rows)
      if    sort == :hash then rows.sort! { |a, b| 2 * (a.sha <=> b.sha) + (a.path <=> b.path) }
      elsif sort == :size then rows.sort! { |a, b| 2 * (b.sz <=> a.sz) + (a.path <=> b.path) }
      elsif sort == :path
        rows.sort! do |a, b|
          a1, b1 = File.basename(a.path), File.basename(b.path)
          a2, b2 = File.dirname(a.path), File.dirname(b.path)
          2 * (a1 <=> b1) + (a2 <=> b2)
        end
      end
    end

    ## Make sure the sort style for displayed data is set to some rational value.
     #
    def ensure_sort(rows)
      return sort if sort
      bnames = rows.map { File.basename(File.expand_path(_1.path)) }
      @sort = bnames.to_set.size < bnames.size ? :path : :hash
    end

    ## Given random results from the worker pool, convert to rows that can be
     # displayed. In this case each file processed has its own record, so those
     # must be grouped back together into the original paths that the user specified.
     #
    def reconsolidate_records(raw_recs)
      shadig = Digest::SHA256.new
      by_path = raw_recs.group_by { _1.path }
      rows = []
      by_path.each do |path, raw_recs|
        shadig.reset
        sz = 0
        ts1, ts2 = Float::INFINITY, 0.0
        raw_recs.sort do |a, b|
          a.idx <=> b.idx
        end.each do
          shadig << _1.sha
          sz += _1.sz
          ts1 = _1.ts1 if _1.ts1 < ts1
          ts2 = _1.ts2 if _1.ts2 > ts2
        end
        rows << RECORD.new(path, 0, 0, shadig.hexdigest, sz, ts1, ts2)
      end
      rows
    end

    ## Build a list of independent asynchronous workers. In this case each worker
     # is a separate process, linked to this process by anonymous kernel pipes.
     #
    def actor_list(pool_sz)
      (0...pool_sz).map do
        TheC::Practor.new.start do |actor|
          shadig = Digest::SHA256.new
          csz = 2**18 # Use 256KiB buffer per actor
          chunk = "".b
          while (path, idx, file = actor.receive) # Nil means all work items have been sent
            shadig.reset
            sz = 0
            ts1 = Time.now.to_f
            File.open(file, "rb") do |io|
              while io.read(csz, chunk)
                sz += chunk.size
                shadig << chunk
              end
            end
            ts2 = Time.now.to_f
            actor.send([path, idx, file, shadig.hexdigest, sz, ts1, ts2])
          end
        end
      end
    end

    ## If any given user paths are dirs, recursively descend them and return their
     # file info here. Each result member is a 3-tuple of:
     # (orig_dir_path, index_within_dir_path, full_file_path)
     #
    def find_file_tuples
      files = []
      paths.each_with_index do |path, idx|
        stat = File.lstat(path)
        unless stat.directory? || stat.file?
          puterr("Skipping: Not a dir or file: #{path.inspect}")
          next
        end
        Find.find(path).select do
          File.lstat(_1).file?
        end.sort.each_with_index do |f, i|
          files << [path, i, f]
        end
      end
      files
    end

    ## Display progress info to terminal screen.
     #
    def progress(file, index, total) = reline "%s (%d/%d)..." % [file, index, total]

    ## Support rewriting the same terminal line for messages.
     #
    def reline(msg)
      c1, c2 = %W[\b \ ].map { _1 * @last_reline }
      @last_reline = (msg[/\n([^\n]*)\z/, 1] || msg).size
      $stderr << c1 << c2 << c1 << msg
    end
  end
end
