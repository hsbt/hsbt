# coding: utf-8

require 'capistrano_colors'
require 'bundler/capistrano'
require 'capistrano-puma'

set :default_environment, {
  'PATH' => "$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
}

set :puma_env, 'production'
set :rails_env, 'production'

require 'pit'
config = Pit.get('whispered', :require => {
    :username => 'your username',
    :server => 'your server address'
  })

set :application, 'whispered'
set :scm, :git
set :repository, File.expand_path('../..', __FILE__)
set :branch, 'master'
set :deploy_via, :copy

server config[:server], :app, :web, :db, :primary => true

set :user, config[:username]
set :deploy_to, defer { "/home/#{user}/app/#{application}" }
set :use_sudo, false

namespace :deploy do
  desc 'update symlink'
  task :update_symlink, :roles => :app do
    require 'pathname'

    ["config/settings/production.local.yml"].each do |path|
      src = "#{shared_path}/system/#{Pathname.new(path).basename.to_s}"
      dest = "#{latest_release}/#{path}"

      run "if [ -e #{dest} -o -h #{dest} ]; then rm -rf #{dest}; fi"
      run "ln -s #{src} #{dest}"
    end
  end

  after 'deploy:finalize_update', 'deploy:update_symlink'
  after 'deploy:update', 'deploy:cleanup'
end
