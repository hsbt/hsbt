require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
  gem "faraday-retry"
end

client = Octokit::Client.new(access_token: ENV.fetch("GITHUB_TOKEN"))
client.auto_paginate = true

excluded_orgs = %w[88labs andpad-dev RubyGemsOrg]
excluded_repos = %w[ruby/vscode-rdbg ruby/gem_rbs_collection ruby/vscode-typeprof]
repos = client.repos(affiliation: "owner,collaborator,organization_member").select { |repo|
  repo.permissions.push && !repo.archived && !excluded_orgs.include?(repo.owner.login) && !excluded_repos.include?(repo.full_name) && !(repo.owner.login == "hsbt" && repo.fork)
}

repos.each do |repo|
  begin
    pulls = client.pull_requests(repo.full_name, state: "open").select { |pr|
      pr.user.login == "dependabot[bot]"
    }
  rescue Octokit::NotFound
    warn "skip #{repo.full_name}: not found"
    next
  end
  next if pulls.empty?

  puts "#{repo.full_name} (#{pulls.size})"
  pulls.each do |pr|
    puts "  ##{pr.number} #{pr.title}"
    puts "    #{pr.html_url}"
    system("open", pr.html_url)
  end
  puts
end
