%w(builder-* post-* provisioner-*).each do |pack|
  Dir.glob(pack).each do |cmd|
    `mv #{cmd} packer-#{cmd}`
  end
end
