load "../../rb/rockly/arma.rake"

require "base64"
require "zlib"

if (ARGV & %w[build deploy package]).any?
  # Special case, always reset this file since `package` modifies it
  touch "lib/the_c.rb"
end

desc "Special packaging for distribution of single file"
task(:package => %i[deploy]) { |t|
  arma.run(t.name) {
    source_dir = "#{arma.run_dir}/#{arma.dist_name}"
    package_lines = begin
      Rly::FsTree.new(source_dir)
      .then { |tree|
        tree.nodes.map { |node|
          next nil if node.lstat.directory?
          [
            node.pathname.to_s,
            is_ln = node.lstat.symlink?,
            node.lstat.mtime.to_f.round(3),
            is_ln ? node.pathname.readlink : File.binread("#{source_dir}/#{node.pathname}")
          ]
        }
      }.tap(&:compact!)
      .then { |files| Marshal.dump(files) }
      .then { |package_raw| Zlib::Deflate.deflate(package_raw, Zlib::BEST_COMPRESSION) }
      .then { |package_z| Base64.urlsafe_encode64(package_z, padding: false) }
      .then { |package|
        result = []
        0.step(package.size - 1, 132).each { |i| result << package[i .. i + 131] }
        result
      }
    end
    File.open("#{source_dir}/the_c.rb", "at") { |io|
      io << "\nTheC_PackageData = [<<END]\n" <<
        package_lines.join("\n") <<
        "\nEND\n\nexit(TheC.bootstrap) if $0 == __FILE__\n"
    }
    arma.log "Added #{package_lines.size} base64 lines to \"the_c.rb\"."
  }
}
