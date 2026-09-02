include_recipe "packages.rb"
include_recipe "timezone.rb"
include_recipe "rustup.rb"
include_recipe "ufw.rb"

# tDiary plugins are placed here by mitamae; tdiary.conf's sp.path points
# to this directory instead of a git checkout of this repository.
plugin_dir = "/home/ubuntu/app/tdiary/shared/hsbt-plugin"

execute "systemctl daemon-reload" do
  action :nothing
end

# Service resources are declared after the files they depend on so a fresh
# machine does not try to start h2o or tdiary before they are configured.

remote_file "/etc/systemd/system/h2o.service" do
  source "files/etc/systemd/system/h2o.service"
  owner "root"
  group "root"
  mode "644"
  notifies :run, "execute[systemctl daemon-reload]", :immediately
  notifies :restart, "service[h2o]"
end

remote_file "/etc/systemd/system/tdiary.service" do
  source "files/etc/systemd/system/tdiary.service"
  owner "root"
  group "root"
  mode "644"
  notifies :run, "execute[systemctl daemon-reload]", :immediately
  notifies :restart, "service[tdiary]"
end

directory "/etc/h2o" do
  owner "root"
  group "root"
  mode "755"
end

# h2o.service only provides /run/h2o; the log directory is ours.
directory "/var/log/h2o" do
  owner "root"
  group "root"
  mode "755"
end

remote_file "/etc/h2o/h2o.conf" do
  source "files/etc/h2o/h2o.conf"
  owner "root"
  group "root"
  mode "644"
  notifies :restart, "service[h2o]"
end

# Document roots served by h2o. index.html and stylesheets/ are uploaded
# by `rake deploy`; diary/ is tDiary's static output. tdiary.conf and
# .htpasswd live in www/ too but stay out of the recipe (secrets).
%w[
  /home/ubuntu/www
  /home/ubuntu/www/hsbt.org
  /home/ubuntu/www/hsbt.org/stylesheets
  /home/ubuntu/www/hsbt.org/diary
  /home/ubuntu/backup
].each do |dir|
  directory dir do
    owner "ubuntu"
    group "ubuntu"
    mode "755"
  end
end

# mruby handler required from h2o.conf; h2o does not ship this one
# (htpasswd.rb comes from /usr/local/share/h2o/mruby).
remote_file "/home/ubuntu/www/rewrite_rules.rb" do
  source "files/www/rewrite_rules.rb"
  owner "ubuntu"
  group "ubuntu"
  mode "644"
  notifies :reload, "service[h2o]"
end

file "/home/ubuntu/www/htpasswd.rb" do
  action :delete
end

remote_file "/etc/logrotate.d/h2o" do
  source "files/etc/logrotate.d/h2o"
  owner "root"
  group "root"
  mode "644"
end

# certbot only runs executable hooks, so mode matters here.
remote_file "/etc/letsencrypt/renewal-hooks/deploy/h2o-reload.sh" do
  source "files/etc/letsencrypt/renewal-hooks/deploy/h2o-reload.sh"
  owner "root"
  group "root"
  mode "755"
end

# Superseded by the deploy hook above; it was never executable anyway.
file "/etc/letsencrypt/renewal-hooks/post/ocsp.sh" do
  action :delete
end

# Validate before reload so a broken drop-in cannot lock us out of SSH.
execute "sshd -t && systemctl reload ssh" do
  action :nothing
end

remote_file "/etc/ssh/sshd_config.d/10-hardening.conf" do
  source "files/etc/ssh/sshd_config.d/10-hardening.conf"
  owner "root"
  group "root"
  mode "644"
  notifies :run, "execute[sshd -t && systemctl reload ssh]"
end

directory plugin_dir do
  owner "ubuntu"
  group "ubuntu"
  mode "755"
end

Dir.glob("#{File.dirname(__FILE__)}/files/tdiary-plugin/*.rb").sort.each do |plugin|
  remote_file "#{plugin_dir}/#{File.basename(plugin)}" do
    source "files/tdiary-plugin/#{File.basename(plugin)}"
    owner "ubuntu"
    group "ubuntu"
    mode "644"
    notifies :restart, "service[tdiary]"
  end
end

# mina only runs `git pull --rebase` here, so the initial clone is ours.
execute "git clone https://github.com/tdiary/tdiary-contrib.git /home/ubuntu/app/tdiary/shared/tdiary-contrib" do
  user "ubuntu"
  not_if "test -d /home/ubuntu/app/tdiary/shared/tdiary-contrib/.git"
end

service "h2o" do
  action [:enable, :start]
end

service "tdiary" do
  action [:enable, :start]
end
