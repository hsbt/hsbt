require "bundler"
require "json"
require "securerandom"
require "rubygems"

lockfile = Bundler::LockfileParser.new(File.read("Gemfile.lock"))

document_name = File.basename(Dir.pwd)
spdx_id = SecureRandom.uuid

sbom = {
  "SPDXID" => "SPDXRef-DOCUMENT",
  "spdxVersion" => "SPDX-2.2",
  "creationInfo" => {
    "created" => Time.now.utc.strftime("%Y-%m-%dT%H:%M:%SZ"),
    "creators" => ["Tool: gemfile_to_sbom"],
    "licenseListVersion" => "3.17"
  },
  "name" => document_name,
  "dataLicense" => "CC0-1.0",
  "documentNamespace" => "https://spdx.org/spdxdocs/#{document_name}-#{spdx_id}",
  "packages" => []
}

lockfile.specs.each do |spec|
  begin
    gemspec = Gem::Specification.find_by_name(spec.name, spec.version)
    
    # 複数ライセンスに対応
    licenses = []
    if gemspec
      if gemspec.license && !gemspec.license.empty?
        licenses << gemspec.license
      end
      
      if gemspec.licenses && !gemspec.licenses.empty?
        licenses.concat(gemspec.licenses)
      end
      
      licenses.uniq!
    end
    
    license_string = licenses.empty? ? "NOASSERTION" : licenses.join(", ")
  rescue Gem::LoadError
    license_string = "NOASSERTION"
  end

  package = {
    "SPDXID" => "SPDXRef-Package-#{spec.name}",
    "name" => spec.name,
    "versionInfo" => spec.version.to_s,
    "downloadLocation" => "NOASSERTION",
    "filesAnalyzed" => false,
    "licenseConcluded" => license_string,
    "licenseDeclared" => license_string,
    "supplier" => "NOASSERTION",
    "externalRefs" => [
      {
        "referenceCategory" => "PACKAGE_MANAGER",
        "referenceType" => "purl",
        "referenceLocator" => "pkg:gem/#{spec.name}@#{spec.version}"
      }
    ]
  }
  sbom["packages"] << package
end

File.write("bom.json", JSON.pretty_generate(sbom))
