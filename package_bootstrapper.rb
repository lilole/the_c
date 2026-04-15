module PackageBootstrapper
  def self.main         = raise "Build must implement: #{__method__.to_s.inspect}"
  def self.package_data = raise "Build must implement: #{__method__.to_s.inspect}"
  def self.unpack_dir   = raise "Build must implement: #{__method__.to_s.inspect}"

  def self.run
    decompress if ! File.directory?(unpack_dir)
    main
    true
  rescue
    $stderr << $!.full_message
    false
  end

  def self.decompress
    require "base64"
    require "fileutils"
    require "pathname"
    require "zlib"

    package_data.delete_at(0)
    .tap { |package_b64| package_b64.tr!("\n", "") }
    .then { |package_b64| Base64.urlsafe_decode64(package_b64) }
    .then { |package_z| Zlib::Inflate.inflate(package_z) }
    .then { |package_raw| Marshal.load(package_raw) }
    .then { |file_tups|
      file_tups.each { |path, is_ln, is_exec, data|
        pn = Pathname.new("#{unpack_dir}/#{path}")
        FileUtils.mkdir_p(pn.parent.to_s)
        if is_ln
          pn.make_symlink(data)
        else
          pn.open("wb") { |io| io.write(data) }
          FileUtils.chmod("+x", pn.to_s) if is_exec
        end
      }
    }
  end
end

# Build task `package` from `package_bootstrapper.rake` adds stuff below here,
# which integrates with module `PackageBootstrapper`...
