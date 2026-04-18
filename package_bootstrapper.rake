
require "base64"
require "zlib"

if (ARGV & %w[build deploy package]).any?
  # Special case, always reset this file since `package` modifies it
  touch "lib/#{arma.let :subject_file}"
end

desc "Special packaging for distribution of single file"
task(package: %i[deploy]) { |t|
  final_file = "#{arma.dist_dir}/#{arma.let :subject_file}"
  bootstrapper_config = <<~END
    def unpack_dir = "\#{ENV["HOME"]}/.bashrc.the_c-#{arma.version}"

    def main
      require_relative "\#{unpack_dir}/aut_aut"
      AutAut.setup(File.realpath(unpack_dir))
      Extensions.apply
      TheC::Cli.new(ARGV)
    end
  END
  bootstrapper_src = "./package_bootstrapper.rb"

  arma.run(t.name) {
    raise "File #{bootstrapper_src.inspect} is required." if ! File.readable?(bootstrapper_src)

    File.open(final_file, "at") { |io|
      io <<
        "\nmodule PackageBootstrapper\n" <<
        "class << self\n" <<
        "#{bootstrapper_config.strip}\n" <<
        "end\n" <<
        "end\n" <<
        "\n\#{{package_data}}\n" <<
        "\nexit(PackageBootstrapper.run) if $0 == __FILE__\n"
    }

    package_b64 = begin
      Ulse::FsTree.new(arma.dist_dir)
      .then { |tree|
        tree.nodes.map { |node|
          next nil if node.lstat.directory?
          [
            node.pathname.to_s,
            is_ln = node.lstat.symlink?,
            node.lstat.executable?,
            is_ln ? node.pathname.readlink : File.binread("#{arma.dist_dir}/#{node.pathname}")
          ]
        }
      }.tap(&:compact!)
      .then { |file_tups| Marshal.dump(file_tups) }
      .then { |package_raw| Zlib::Deflate.deflate(package_raw, Zlib::BEST_COMPRESSION) }
      .then { |package_z| Base64.urlsafe_encode64(package_z, padding: false) }
      .tap { |package_b64|
        i = (package_b64.size - 1) / 132
        package_b64.insert((i + 1) * 132, "\n") while (i -= 1) >= 0
      }
    end

    File.read(final_file).lines
    .map! { |line|
      if line =~ /^\s*#\{\{package_bootstrapper}}\s*$/
        File.read(bootstrapper_src)
      elsif line =~ /^\s*#\{\{package_data}}\s*$/
        "def PackageBootstrapper.package_data\n[+(<<END)]\n#{package_b64}\nEND\nend\n"
      else
        line
      end
    }.join
    .then { |body| File.write(final_file, body) }

    arma.log "Added #{package_b64.size} base64 bytes to #{final_file.inspect}."
    cp final_file, "."
    arma.log "Full bootstrapper file: #{File.basename(final_file).inspect}"
  }
}
