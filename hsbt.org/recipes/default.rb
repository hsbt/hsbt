# tDiary plugins are placed here by mitamae; tdiary.conf's sp.path points
# to this directory instead of a git checkout of this repository.
plugin_dir = "/home/ubuntu/app/tdiary/shared/hsbt-plugin"

execute "systemctl daemon-reload" do
  action :nothing
end

service "h2o" do
  action [:enable, :start]
end

service "tdiary" do
  action [:enable, :start]
end

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

remote_file "/etc/h2o/h2o.conf" do
  source "files/etc/h2o/h2o.conf"
  owner "root"
  group "root"
  mode "644"
  notifies :restart, "service[h2o]"
end

remote_file "/etc/logrotate.d/h2o" do
  source "files/etc/logrotate.d/h2o"
  owner "root"
  group "root"
  mode "644"
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
