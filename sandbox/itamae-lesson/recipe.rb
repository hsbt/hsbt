execute 'yum update' do
  command 'sudo yum update -y'
end

execute 'groupinstall' do
  command 'sudo yum groupinstall "Development Tools" -y'
end

package 'openssl-devel'
package 'zlib-devel'

user 'chkbuild' do
  username "chkbuild"
  password "chkbuild"
end

package 'mysql-server'
package 'mysql-devel'

execute 'rbenv clone' do
  command "git clone https://github.com/sstephenson/rbenv.git ~chkbuild/.rbenv; echo 'export PATH=\"$HOME/.rbenv/bin:$PATH\"' >> ~chkbuild/.bash_profile; echo 'eval \"$(rbenv init -)\"' >> ~chkbuild/.bash_profile"
  not_if 'test -d ~chkbuild/.rbenv'
end

execute 'ruby-build' do
  command "git clone https://github.com/sstephenson/ruby-build.git ~chkbuild/.rbenv/plugins/ruby-build"
  not_if 'test -d ~chkbuild/.rbenv/plugins/ruby-build'
end

execute 'ruby 2.1.4' do
  command "~chkbuild/.rbenv/plugins/ruby-build/bin/ruby-build 2.1.4 ~chkbuild/.rbenv/versions/2.1.4"
  not_if 'test -d ~chkbuild/.rbenv/versions/2.1.4'
end

execute 'rbenv chown' do
  command 'chown -R chkbuild. ~chkbuild/.rbenv'
  only_if 'test -d ~chkbuild/.rbenv'
end

