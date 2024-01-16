require "mina/deploy"
require "mina/git"
require "mina/rbenv"
require "mina/bundler"

set :domain, "www.hsbt.org"
set :user, "ubuntu"
set :deploy_to, "/home/#{fetch(:user)}/app/tdiary"
set :repository, "https://github.com/tdiary/tdiary-core.git"
set :branch, "master"

set :rbenv_path, "/home/#{fetch(:user)}/.rbenv"

set :bundle_bin, "#{fetch(:rbenv_path)}/shims/bundle"
set :bundle_withouts, "development test server"
set :bundle_options, -> { %(--without #{fetch(:bundle_withouts)}) }

task :environment do
  invoke :"rbenv:load"
end

task :deploy do
  deploy do
    invoke :"git:clone"
    invoke :"deploy:cleanup"

    on :launch do
      command "echo \"gem 'tdiary-style-gfm', '>= 1.2.0'; gem 'commonmarker', '< 1.0'; gem 'holiday_japan'; gem 'oga'; gem 'rexml'; gem 'rss'\" > #{fetch(:current_path)}/Gemfile.local"
      %w[tdiary-contrib hsbt].each do |dir|
        command "cd #{fetch(:shared_path)}/#{dir}; git pull --rebase"
      end
      command "ln -s /home/#{fetch(:user)}/www/tdiary.conf #{fetch(:current_path)}/tdiary.conf"

      in_path(fetch(:current_path)) do
        invoke :"bundle:install"
        command "chmod 666 Gemfile.lock"
        command "sudo systemctl restart h2o"
      end
    end
  end
end
