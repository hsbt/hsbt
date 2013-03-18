load 'deploy'

require 'capistrano_colors'

require 'pit'
config = Pit.get('whispered', :require => {
    :username => 'your username',
    :server => 'your server address'
  })

set :application, 'whispered-puppet'
set :scm, :git
set :repository, File.expand_path('..', __FILE__)
set :branch, 'master'
set :deploy_via, :copy

server config[:server], :app, :web, :db, :primary => true

set :user, config[:username]
set :deploy_to, defer { "/home/#{user}/app/#{application}" }
set :use_sudo, false

after 'deploy:update', 'deploy:cleanup'
