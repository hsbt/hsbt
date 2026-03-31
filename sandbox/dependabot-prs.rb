require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
end

client = Octokit::Client.new(access_token: ENV.fetch("GITHUB_TOKEN"))
client.auto_paginate = true

excluded_orgs = %w[88labs andpad-dev]
repos = client.repos(affiliation: "owner,collaborator,organization_member").select { |repo|
  repo.permissions.push && !repo.archived && !excluded_orgs.include?(repo.owner.login) && !(repo.owner.login == "hsbt" && repo.fork)
}

repos.each do |repo|
  pulls = client.pull_requests(repo.full_name, state: "open").select { |pr|
    pr.user.login == "dependabot[bot]"
  }
  next if pulls.empty?

  puts "#{repo.full_name} (#{pulls.size})"
  pulls.each do |pr|
    puts "  ##{pr.number} #{pr.title}"
    puts "    #{pr.html_url}"
    system("open", pr.html_url)
  end
  puts
end
