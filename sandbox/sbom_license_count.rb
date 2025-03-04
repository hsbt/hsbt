#!/usr/bin/env ruby
# Script to extract license information from SBOM file and display license usage counts

require "json"

# Load bom.json by default, or a file specified as an argument
filename = ARGV[0] || "bom.json"

begin
  sbom = JSON.parse(File.read(filename))
rescue Errno::ENOENT
  puts "Error: File '#{filename}' not found"
  exit 1
rescue JSON::ParserError
  puts "Error: '#{filename}' is not a valid JSON file"
  exit 1
end

# Count license information
license_count = Hash.new(0)

sbom["packages"].each do |package|
  license = package["licenseDeclared"]
  
  # Handle multiple licenses separated by commas
  if license.include?(",")
    licenses = license.split(",").map(&:strip)
    licenses.each do |lic|
      license_count[lic] += 1
    end
  else
    license_count[license] += 1
  end
end

# Sort by usage count (descending)
sorted_licenses = license_count.sort_by { |_, count| -count }

puts "=== License Usage in SBOM File ==="
puts "File: #{filename}"
puts "Total packages: #{sbom["packages"].size}"
puts

# Display license usage counts
sorted_licenses.each do |license, count|
  puts "#{license}: #{count} package(s)"
end

puts

# Display packages by license
puts "=== Packages by License ==="
sorted_licenses.each do |license, _|
  packages = sbom["packages"].select do |package|
    if package["licenseDeclared"].include?(",")
      package["licenseDeclared"].split(",").map(&:strip).include?(license)
    else
      package["licenseDeclared"] == license
    end
  end
  
  puts "\n#{license} (#{packages.size} package(s)):"
  packages.each do |package|
    puts "  - #{package["name"]} (#{package["versionInfo"]})"
  end
end