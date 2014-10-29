execute 'groupinstall' do
  command 'sudo yum groupinstall "Development Tools"'
end

package 'openssl-devel'
package 'zlib-devel'
