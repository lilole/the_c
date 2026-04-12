# frozen_string_literal: true

module TheC
module Ffg
  class Configuration
    attr_accessor :ctxt, :dot_dirs, :fg, :keep_dev, :path_args, :path_only, :quiet, :re_arg, :re_opts,
      :skip_res, :src_dir_names, :src_dirs

    def initialize
      @ctxt = @re_opts = 0
      @dot_dirs = @fg = @path_only = @quiet = @src_dirs = false
      @keep_dev      = true
      @path_args     = []
      @skip_res      = %w[/(tmp|log|coverage|(r?spec|tests?)/fixtures)$]
      @src_dir_names = %w[.git .svn CVS].to_set
    end
  end
end
end
