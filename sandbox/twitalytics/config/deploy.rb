require 'bundler/capistrano'
require 'capistrano_colors'

server "localhost", :app, :db, primary: true
ssh_options[:port] = 2222
ssh_options[:keys] = "~/.vagrant.d/insecure_private_key"

set :user, "vagrant"
set :group, "vagrant"
set :use_sudo, false

set :deploy_to, "/opt/trinidad"
set :application, "twitalytics"
set :repository, "."
set :scm, :none
set :deploy_via, :copy
set :copy_exclude, [".git", "log", "tmp", "*.box", "*.war", ".idea", ".DS_Store"]

set :default_environment, "PATH" => "/opt/jruby/bin:$PATH", "JSVC_ARGS_EXTRA" => "-user vagrant"
set :bundle_dir, ""
set :bundle_flags, "--system --quiet"

before "deploy:setup", "deploy:install_bundler"
namespace :deploy do
  task :install_bundler, :roles => :app do
    run "sudo gem install bundler --pre"
  end
  task :start, :roles => :app do
    run "/etc/init.d/trinidad start"
  end
  task :stop, :roles => :app do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "touch #{File.join(current_path,'tmp','restart.txt')}"
  end
end
