require "rake"
require "fileutils"

desc 'generate index.html'
task :generate do
  require 'kramdown'
  header = File.read('hsbt.org/src/header.html')
  body = Kramdown::Document.new(File.read('hsbt.org/src/index.md')).to_html
  footer = File.read('hsbt.org/src/footer.html')
  File.open('hsbt.org/dist/index.html', 'w') do |f|
    [header, body, footer].each {|s| f.puts s}
  end

  require 'sassc'
  sass = File.read('hsbt.org/src/hsbt.scss')
  File.open('hsbt.org/dist/hsbt.css', 'w') do |f|
    f.puts SassC::Engine.new(sass, style: :compressed).render
  end
end

desc 'deploy index.html'
task :deploy => [:generate] do
  require 'net/scp'
  Net::SCP.upload!('www.hsbt.org', 'ubuntu', "hsbt.org/dist/index.html", "/home/ubuntu/www/hsbt.org/index.html")
  Net::SCP.upload!('www.hsbt.org', 'ubuntu', "hsbt.org/dist/hsbt.css", "/home/ubuntu/www/hsbt.org/stylesheets/hsbt.css")
end

XDG_CONFIG_FILES = Dir.glob(File.join("toolbox", "xdg", "config", "**", "*")).select{|f| !File.directory?(f)}

task :push do
  XDG_CONFIG_FILES.each do |f|
    from = f
    to = f.gsub(/toolbox\/xdg\/config/, ENV['XDG_CONFIG_HOME'])
    FileUtils.cp from, to
  end

  system "sudo cp toolbox/system/paths /etc/paths.d/paths"
end

task :pull do
  XDG_CONFIG_FILES.each do |f|
    to = f
    from = f.gsub(/toolbox\/xdg\/config/, ENV['XDG_CONFIG_HOME'])
    FileUtils.cp from, to
  end

  system "sudo cp /etc/paths.d/paths toolbox/system/paths"
end
