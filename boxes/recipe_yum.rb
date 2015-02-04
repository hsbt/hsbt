execute 'yumupdate' do
  command 'sudo yum update -y'
end

execute 'groupinstall' do
  command 'sudo yum groupinstall "Development Tools" -y'
  command 'sudo yum install openssl-devel zlib-devel -y'
end
