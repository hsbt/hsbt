# tDiary plugins are placed here by mitamae; tdiary.conf's sp.path points
# to this directory instead of a git checkout of this repository.
plugin_dir = "/home/ubuntu/app/tdiary/shared/hsbt-plugin"

service "tdiary" do
  action :nothing
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
