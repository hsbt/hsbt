tags = `git tag`
tags.split.each{|tag| `git tag -d #{tag}`}
