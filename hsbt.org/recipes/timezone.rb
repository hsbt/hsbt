execute "timedatectl set-timezone Asia/Tokyo" do
  not_if 'test "$(timedatectl show -p Timezone --value)" = Asia/Tokyo'
end
