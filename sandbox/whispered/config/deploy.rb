# coding: utf-8

require 'capistrano_colors'
require 'bundler/capistrano'

set :default_environment, {
  'PATH' => "$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH"
}

require 'pit'
require_params = {
  :username => 'your username',
  :server => 'your server address'
}
config = Pit.get('whispered', :require => require_params)

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

set :puma_env, 'production'
set :rails_env, 'production'
set :stage, 'production'
set :shared_children, shared_children << 'tmp/sockets'

puma_sock    = "unix://#{shared_path}/sockets/puma.sock"
puma_control = "unix://#{shared_path}/sockets/pumactl.sock"
puma_state   = "#{shared_path}/sockets/puma.state"
puma_log     = "#{shared_path}/log/puma-#{stage}.log"

namespace :deploy do
  desc "Start the application"
  task :start do
    run "cd #{current_path} && RAILS_ENV=#{stage} && bundle exec puma -d -b '#{puma_sock}' -e #{stage} -t2:4 --control '#{puma_control}' -S #{puma_state} >> #{puma_log} 2>&1 &", :pty => false
  end

  desc "Stop the application"
  task :stop do
    run "cd #{current_path} && RAILS_ENV=#{stage} && bundle exec pumactl -S #{puma_state} stop"
  end

  desc "Restart the application"
  task :restart, :roles => :app, :except => { :no_release => true } do
    stop
    start
  end
end
