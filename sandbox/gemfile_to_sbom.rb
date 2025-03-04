require "bundler"
require "json"
require "securerandom"

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
  package = {
    "SPDXID" => "SPDXRef-Package-#{spec.name}",
    "name" => spec.name,
    "versionInfo" => spec.version.to_s,
    "downloadLocation" => "NOASSERTION",
    "filesAnalyzed" => false,
    "licenseConcluded" => "NOASSERTION",
    "licenseDeclared" => "NOASSERTION",
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
