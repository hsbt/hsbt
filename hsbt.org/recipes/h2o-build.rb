# h2o is built from upstream master because the deb was dropped from
# Ubuntu. Bump h2o_sha to upgrade: `h2o --version` embeds the short sha
# it was built from, so every step below is skipped while it matches.
# Listen changes and start_server changes still need a manual restart.
h2o_sha = "706842c0f8c0d9422efb97a4d8ef7d6ec9df87b7"
src = "/home/ubuntu/build/h2o"
# mruby's build needs a ruby; the rbenv shims hardcode RBENV_ROOT so they
# also work for the root-run install step.
build_path = "/home/ubuntu/.rbenv/shims:/usr/local/bin:/usr/bin:/bin"
installed = "test -x /usr/local/bin/h2o && /usr/local/bin/h2o --version | grep -q '@#{h2o_sha[0, 7]}'"

service "h2o" do
  action :nothing
end

directory "/home/ubuntu/build" do
  owner "ubuntu"
  group "ubuntu"
  mode "755"
end

execute "git clone https://github.com/h2o/h2o.git #{src}" do
  user "ubuntu"
  not_if "test -d #{src}/.git"
end

execute "git -C #{src} fetch origin && git -C #{src} checkout --detach #{h2o_sha}" do
  user "ubuntu"
  not_if "test \"$(git -C #{src} rev-parse HEAD)\" = #{h2o_sha}"
end

execute "env PATH=#{build_path} cmake -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -DWITH_MRUBY=ON ." do
  cwd src
  user "ubuntu"
  not_if installed
end

execute "env PATH=#{build_path} make -C build -j$(nproc) h2o" do
  cwd src
  user "ubuntu"
  not_if installed
end

execute "env PATH=#{build_path} make -C build install" do
  cwd src
  not_if installed
  notifies :reload, "service[h2o]"
end
