# Sorry this is not public yet...
load "../ultisel/load/arma.rake"

arma.import arma: "../ultisel", version: nil, build: true,
  include: /^aut_aut\.rb/

arma.plugin :Package,
  subject_root: "lib",
  subject_file: "the_c.rb",
  unpack_dir_body: (<<~END),
    "\#{ENV["HOME"]}/.bashrc.the_c-#{arma.version}"
  END
  main_body: (<<~'END')
    require_relative "#{unpack_dir}/aut_aut"
    AutAut.setup(File.realpath(unpack_dir))
    Extensions.apply
    TheC::Cli.new(ARGV)
  END
