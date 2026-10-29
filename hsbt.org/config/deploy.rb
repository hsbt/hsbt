require "mina/deploy"
require "mina/git"
require "mina/rbenv"
require "mina/bundler"

# Ruby on Windows seeds ENV["USER"] with the local account name, and Mina's
# Configuration#fetch prefers ENV over `set`, hijacking the SSH user.
ENV.delete("USER")

set :domain, "www.hsbt.org"
set :user, "ubuntu"
set :deploy_to, "/home/#{fetch(:user)}/app/tdiary"
set :repository, "https://github.com/tdiary/tdiary-core.git"
set :branch, "master"

set :rbenv_path, "/home/#{fetch(:user)}/.rbenv"

set :bundle_bin, "#{fetch(:rbenv_path)}/shims/bundle"
set :bundle_withouts, "development test server"
set :bundle_options, -> { }

task :environment do
  invoke :"rbenv:load"
end

desc "Generates dist/index.html from src/"
task :"www:generate" do
  require "kramdown"
  header = File.read("src/header.html")
  body = Kramdown::Document.new(File.read("src/index.md")).to_html
  footer = File.read("src/footer.html")
  File.open("dist/index.html", "w") do |f|
    [header, body, footer].each { |s| f.puts s }
  end
end

# The index page lives beside tDiary, not in its releases, so each deploy
# overwrites it in place. Net::SCP runs outside mina's command queue, hence
# the explicit --simulate check.
desc "Uploads dist/index.html and dist/hsbt.css"
task :"www:upload" => :"www:generate" do
  www = "/home/#{fetch(:user)}/www/hsbt.org"
  files = {
    "dist/index.html" => "#{www}/index.html",
    "dist/hsbt.css" => "#{www}/stylesheets/hsbt.css",
  }
  if fetch(:simulate)
    files.each { |src, dest| puts "# scp #{src} #{fetch(:domain)}:#{dest}" }
  else
    require "net/scp"
    Net::SCP.start(fetch(:domain), fetch(:user)) do |scp|
      files.each { |src, dest| scp.upload!(src, dest) }
    end
  end
end

task :deploy do
  invoke :"www:upload"

  deploy do
    invoke :"git:clone"
    invoke :"deploy:cleanup"

    on :launch do
      gemfile_local_gems = %w[
        tdiary-style-gfm
        logger
        fcgi
        holiday_japan
        rss
      ]
      command "printf \"gem '%s'\\n\" #{gemfile_local_gems.join(" ")} > #{fetch(:current_path)}/Gemfile.local"
      # hsbt plugins are placed into shared/hsbt-plugin by mitamae
      # (recipes/default.rb), not pulled from a git checkout here.
      %w[tdiary-contrib].each do |dir|
        command "cd #{fetch(:shared_path)}/#{dir}; git pull --rebase"
      end
      command "ln -s /home/#{fetch(:user)}/www/tdiary.conf #{fetch(:current_path)}/tdiary.conf"

      in_path(fetch(:current_path)) do
        command "sed -i \"/gem 'tdiary-style-gfm'/d\" Gemfile"
        command "#{fetch(:bundle_bin)} config set without '#{fetch(:bundle_withouts)}'"
        invoke :"bundle:install"
        command "chmod 666 Gemfile.lock"
        command "sudo systemctl restart tdiary"
        command "sudo systemctl restart h2o"
      end
    end
  end
end
