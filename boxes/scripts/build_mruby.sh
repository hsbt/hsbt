#!/usr/bin/env bash

if [ ! -f ~/.rbenv ]; then
  git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
  rm ~/.bash_profile
  echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bash_profile
  echo 'eval "$(rbenv init -)"' >> ~/.bash_profile
  git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
  source ~/.bash_profile
  rbenv install 2.1.5
fi

rbenv shell 2.1.5

sudo yum install pcre pcre-devel -y

cp /vagrant/build_config.rb /vagrant/github.com/matsumoto-r/ngx_mruby/
cd /vagrant/github.com/matsumoto-r/ngx_mruby/
./configure --with-ngx-src-root=/vagrant/github.com/nginx/nginx
make build_mruby
make generate_gems_config

cd /vagrant/github.com/nginx/nginx
./configure --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp --http-scgi-temp-path=/var/cache/nginx/scgi_temp --user=nginx --group=nginx --with-http_ssl_module --with-http_realip_module --with-http_addition_module --with-http_sub_module --with-http_dav_module --with-http_flv_module --with-http_mp4_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_random_index_module --with-http_secure_link_module --with-http_stub_status_module --with-mail --with-mail_ssl_module --with-file-aio --with-ipv6 --with-cc-opt='-O2 -g -pipe -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=4 -m64 -mtune=generic' --add-module=/vagrant/github.com/matsumoto-r/ngx_mruby --add-module=/vagrant/github.com/matsumoto-r/ngx_mruby/dependence/ngx_devel_kit
make

cd objs
tar czf nginx.tar.gz nginx
mv nginx.tar.gz /vagrant
