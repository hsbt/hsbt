#!/usr/bin/env ruby

require 'octokit'
require 'pit'
require_params = {
  :login => 'login',
  :password => 'password'
}
config = Pit.get('github', :require => require_params)

client = Octokit::Client.new(:login => config[:login], :password => config[:password])

issue = client.issue(ARGV[0], ARGV[1].to_i)

title = issue.title
body = [issue.user.login, issue.body].join(': ')
cbody = client.issue_comments(ARGV[0], ARGV[1].to_i).map{|comment| [comment.user.login, comment.body].join(': ')}.join("\n\n----\n\n")

File.open(ARGV[1] + '.txt', 'w'){|f| f.write(title + "\n\n----\n\n" + body + "\n\n----\n\n" + cbody) }
