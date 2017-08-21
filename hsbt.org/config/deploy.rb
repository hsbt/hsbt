require 'mina/deploy'
require 'mina/git'
require 'mina/rbenv'
require 'mina/bundler'

set :domain, "www.hsbt.org"
set :user, "hsbt"
set :deploy_to, "/home/#{fetch(:user)}/app/tdiary"
set :repository, 'git://github.com/tdiary/tdiary-core.git'
set :branch, 'master'

set :rbenv_path, "/home/#{fetch(:user)}/.anyenv/envs/rbenv"

set :bundle_bin, "#{fetch(:rbenv_path)}/shims/bundle"
set :bundle_withouts, "deveploment test server"
set :bundle_options, -> { %{--without #{fetch(:bundle_withouts)}} }

task :environment do
  invoke :'rbenv:load'
end

task :deploy do
  deploy do
    invoke :'git:clone'
    invoke :'deploy:cleanup'

    on :launch do
      command "echo \"gem 'tdiary-style-gfm', '>= 0.5.0'; gem 'oga'\" > #{fetch(:current_path)}/Gemfile.local"
      command "cp -r #{fetch(:shared_path)}/lib/* #{fetch(:current_path)}/misc/lib"
      command "cp -r #{fetch(:shared_path)}/js/* #{fetch(:current_path)}/js"
      command "ln -s /home/hsbt/www/tdiary.conf #{fetch(:current_path)}/tdiary.conf"

      in_path(fetch(:current_path)) do
        invoke :'bundle:install'
        command "chmod 666 Gemfile.lock"
        command 'sudo systemctl restart h2o'
      end
    end
  end
end
