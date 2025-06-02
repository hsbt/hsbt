require 'rubygems/command'
require 'rubygems/remote_fetcher'
require 'json'
require 'net/http'
require 'uri'

class Gem::Commands::CloneCommand < Gem::Command
  def initialize
    super 'clone', 'Clone a gem repository using ghq'

    add_option('-v', '--verbose', 'Show verbose output') do |value, options|
      options[:verbose] = value
    end
  end

  def arguments # :nodoc:
    "GEM_NAME        name of gem to clone"
  end

  def description # :nodoc:
    <<-EOF
The clone command fetches gem metadata from RubyGems.org and clones
the gem's source repository using ghq based on the homepage or
source_code_uri from the gem's metadata.

Examples:
  gem clone sinatra
  gem clone rails --verbose
    EOF
  end

  def execute
    gem_name = get_one_gem_name

    say "Fetching gem metadata for '#{gem_name}'..." if options[:verbose]

    gem_info = fetch_gem_info(gem_name)

    if gem_info.nil?
      alert_error "Could not find gem '#{gem_name}'"
      terminate_interaction 1
    end

    repository_url = extract_repository_url(gem_info)

    if repository_url.nil?
      alert_error "Could not find repository URL for gem '#{gem_name}'"
      terminate_interaction 1
    end

    say "Found repository URL: #{repository_url}" if options[:verbose]

    clone_repository(repository_url)
  end

  private

  def fetch_gem_info(gem_name)
    uri = URI("https://rubygems.org/api/v1/gems/#{gem_name}.json")

    begin
      response = Net::HTTP.get_response(uri)

      if response.code == '200'
        JSON.parse(response.body)
      else
        nil
      end
    rescue => e
      say "Error fetching gem info: #{e.message}" if options[:verbose]
      nil
    end
  end

  def extract_repository_url(gem_info)
    if gem_info['source_code_uri'] && !gem_info['source_code_uri'].empty?
      return gem_info['source_code_uri']
    end

    if gem_info['homepage_uri'] && is_repository_url?(gem_info['homepage_uri'])
      return gem_info['homepage_uri']
    end

    nil
  end

  def is_repository_url?(url)
    return false if url.nil? || url.empty?

    url.match?(/github\.com|gitlab\.com|bitbucket\.org|codeberg\.org|sourcehut\.org/)
  end

  def clone_repository(url)
    command = "ghq get #{url}"
    say "Executing: #{command}" if options[:verbose]

    system(command)

    if $?.success?
      say "Successfully cloned repository: #{url}"
    else
      alert_error "Failed to clone repository. Make sure 'ghq' is installed and available in your PATH."
      terminate_interaction 1
    end
  end
end
