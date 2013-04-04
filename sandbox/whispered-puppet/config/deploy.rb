require 'mina/git'

require 'pit'
config = Pit.get('whispered', :require => {
    :username => 'your username',
    :server => 'your server address'
  })

set :domain, config[:server]
set :user, config[:username]
set :deploy_to, "/home/#{user}/app/whispered-puppet"
set :repository, 'git://github.com/hsbt/whispered-puppet.git'
set :branch, 'master'

desc "Deploys the current version to the server."
task :deploy do
  deploy do
    invoke :'git:clone'
  end
end
