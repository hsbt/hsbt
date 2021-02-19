#!/usr/bin/env ruby

require 'pit'
require 'octokit'

config = Pit.get('github', :require => {
  'username' => 'Your username of GitHub Enterprise',
  'password' => 'Your password of GitHub Enterprise',
})

octokit = Octokit::Client.new(
  login:    config['username'],
  password: config['password'],
)

octokit.orgs.each do |org|
  octokit.org_repos(org.login).each do |repo|
    puts "Watching #{repo.full_name} ..."
    octokit.update_subscription(repo.full_name, {subscribed: true})
  end
end
