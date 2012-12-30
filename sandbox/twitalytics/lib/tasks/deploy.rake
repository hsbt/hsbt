require 'bundler'
Bundler.require(:deploy)

SSH_KEY = "~/.vagrant.d/insecure_private_key"

def with_ssh
  Net::SSH.start("localhost", "vagrant", {port: 2222, keys: [SSH_KEY]}) do |ssh|
    yield ssh
  end
end

def scp_upload(local_file, remote_file)
  Net::SCP.start("localhost", "vagrant", {port: 2222, keys: [SSH_KEY]}) do |ssh|
    ssh.upload!(local_file, remote_file) do |ch, name, sent, total|
      print "\rCopying #{name}: #{sent}/#{total}"
    end
  end
  print "\n"
end

namespace :deploy do
  desc "Package the application into a WAR file and deploy it"
  task :war do
    Warbler::Task.new(:warble)
    Rake::Task["warble"].invoke
    with_ssh do |ssh|
      ssh.exec! "mkdir -p deploy"
      ssh.exec! "rm -rf deploy/*"
    end

    scp_upload("twitalytics.war", "deploy/")

    with_ssh do |ssh|
      ssh.exec <<-SH do |ch, stream, data|
cd deploy
export PATH=$PATH:/opt/jruby/bin
export RAILS_ENV=production
sudo jgem install warbler-exec
jruby -S warbler-exec twitalytics.war bin/rake db:migrate
SH
        print data
      end
    end

    with_ssh do |ssh|
      ssh.exec! "sudo mv deploy/twitalytics.war /var/lib/tomcat6/webapps/"
      puts "Deployment complete!"
    end
  end
end
