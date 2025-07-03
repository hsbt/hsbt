execute "yumupdate" do
  command "sudo yum update -y"
end

execute "groupinstall" do
  command 'sudo yum groupinstall "Development Tools" -y'
end

execute "ruby-devel" do
  command "sudo yum install openssl-devel zlib-devel -y"
end
