execute 'yumupdate' do
  command 'sudo yum update -y'
end

execute 'groupinstall' do
  command 'sudo yum groupinstall "Development Tools" -y'
end

package 'openssl-devel'
package 'zlib-devel'
