# Only the listeners sshd and h2o expose; everything else this server does
# is outbound (ufw allows that by default) or multiplexed inside SSH.
# allow runs before enable so the session applying this recipe survives.
%w[22/tcp 80/tcp 443/tcp 443/udp].each do |port|
  execute "ufw allow #{port}" do
    not_if "ufw show added | grep -Fxq 'ufw allow #{port}'"
  end
end

execute "ufw --force enable" do
  only_if "ufw status | grep -q inactive"
end
