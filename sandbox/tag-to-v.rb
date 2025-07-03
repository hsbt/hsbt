Dir.chdir("tdiary-core") do
  tags = `git tag`.split.reject { |t| t =~ /^v/ }
  tags.each do |t|
    `git checkout #{t}`
    `git tag v#{t}`
    `git push --tags`
    `git tag -d #{t}`
    `git push origin :#{t}`
  end
end
