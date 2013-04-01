require 'mina/bundler'
require 'mina/rails'
require 'mina/git'
require 'mina/rbenv'

require 'pit'
require_params = {
  :username => 'your username',
  :server => 'your server address'
}
config = Pit.get('whispered', :require => require_params)

set :domain, config[:server]
set :user, config[:username]
set :deploy_to, "/home/#{user}/app/whispered"
set :repository, 'git://github.com/hsbt/whispered.git'
set :branch, 'master'
set :stage, 'production'
set :shared_paths, ['config/database.yml', 'config/settings/production.local.yml', 'log']

task :environment do
  invoke :'rbenv:load'
end

task :setup => :environment do
  queue! %[mkdir -p "#{deploy_to}/shared/log"]
  queue! %[chmod g+rx,u+rwx "#{deploy_to}/shared/log"]

  queue! %[mkdir -p "#{deploy_to}/shared/config"]
  queue! %[chmod g+rx,u+rwx "#{deploy_to}/shared/config"]

  queue! %[touch "#{deploy_to}/shared/config/database.yml"]
  queue  %[echo "-----> Be sure to edit 'shared/config/database.yml'."]
end

set :puma_sock,    "unix://#{deploy_to}/shared/sockets/puma.sock"
set :puma_control, "unix://#{deploy_to}/shared/sockets/pumactl.sock"
set :puma_state,   "#{deploy_to}/shared/sockets/puma.state"
set :puma_log,     "#{deploy_to}/shared/log/puma-#{stage}.log"

task :start => :environment do
  queue! %[cd #{deploy_to}/current && RAILS_ENV=#{stage} && bundle exec puma -d -b '#{puma_sock}' -e #{stage} -t2:4 --control '#{puma_control}' -S #{puma_state} >> #{puma_log} 2>&1 &]
end

task :stop => :environment do
  queue! %[cd #{deploy_to}/current && RAILS_ENV=#{stage} && bundle exec pumactl -S #{puma_state} stop]
end

desc "Deploys the current version to the server."
task :deploy => :environment do
  deploy do
    invoke :'git:clone'
    invoke :'deploy:link_shared_paths'
    invoke :'bundle:install'
    invoke :'rails:assets_precompile'

    to :launch do
      invoke :stop
      invoke :start
    end
  end
end
