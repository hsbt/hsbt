changelog = Hash.new

open('ChangeLog').each do |line|
  if line =~ /^(\d{4}\-\d{2}\-\d{2})/
    changelog[$1] = ""
  else
    changelog[changelog.keys.last] << line
  end
end

changelog.each {|k, v| system "echo '#{v}' | dayone -d='#{k}' new" }
