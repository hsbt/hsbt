require "mina/deploy"
require "mina/git"
require "mina/rbenv"
require "mina/bundler"

# Ruby on Windows seeds ENV["USER"] with the local account name, and Mina's
# Configuration#fetch prefers ENV over `set`, hijacking the SSH user.
ENV.delete("USER")

set :domain, "www.hsbt.org"
set :user, "ubuntu"
set :deploy_to, "/home/#{fetch(:user)}/app/tdiary"
set :repository, "https://github.com/tdiary/tdiary-core.git"
set :branch, "master"

set :rbenv_path, "/home/#{fetch(:user)}/.rbenv"

set :bundle_bin, "#{fetch(:rbenv_path)}/shims/bundle"
set :bundle_withouts, "development test server"
set :bundle_options, -> { }

task :environment do
  invoke :"rbenv:load"
end

desc "Generates dist/index.html from src/"
task :"www:generate" do
  require "kramdown"
  require "erb"
  require "fileutils"
  header = File.read("src/header.html")
  footer = File.read("src/footer.html")
  page = ->(path, markdown) do
    File.open(path, "w") do |f|
      [header, Kramdown::Document.new(markdown).to_html, footer].each { |s| f.puts s }
    end
  end
  index = File.read("src/index.md")
  page.("dist/index.html", index)

  # The talk title and event for each slide come from its entry on the index
  # page. The last entry wins, so a talk listed under both 表彰 and 講演 takes
  # the 講演 one.
  talks = index.scan(%r{^\* (\d{4})年(\d\d)月 (.*?) ?\[(.+?)\]\(/slide/([^/)]+)/\)$})
    .to_h { |year, month, event, title, name| [name, {date: "#{year}年#{month}月", event: event, title: title.gsub(/\\(.)/, '\1')}] }

  slides = Dir.glob("slide/*.pdf").map { |pdf| File.basename(pdf, ".pdf") }.sort.reverse
  viewer = ERB.new(File.read("src/slide.html.erb"))
  FileUtils.mkdir_p("dist/og")
  slides.each do |name|
    image = "dist/og/#{name}.jpg"
    unless File.exist?(image) && File.mtime(image) >= File.mtime("slide/#{name}.pdf")
      system("pdftoppm", "-f", "1", "-singlefile", "-jpeg", "-jpegopt", "quality=85",
        "-scale-to-x", "1200", "-scale-to-y", "-1", "slide/#{name}.pdf", image.delete_suffix(".jpg"), exception: true)
    end
    talk = talks.fetch(name, {date: "", event: "", title: name})
    vars = {
      name: name,
      title: talk[:title],
      description: [talk[:event], talk[:date]].reject(&:empty?).join(" / "),
      url: "https://#{fetch(:domain)}/slide/#{name}/",
      image: "https://#{fetch(:domain)}/slide/#{name}.jpg",
    }.transform_values { |v| ERB::Util.h(v) }
    FileUtils.mkdir_p("dist/slide/#{name}")
    File.write("dist/slide/#{name}/index.html", viewer.result_with_hash(**vars))
  end
  list = slides.map do |name|
    "* [#{name.sub(/\A(\d{4})(\d\d)(\d\d)-/, '\1-\2-\3 ')}](/slide/#{name}/)"
  end
  page.("dist/slide/index.html", ["# 登壇資料", "", *list].join("\n"))
end

# The index page lives beside tDiary, not in its releases, so each deploy
# overwrites it in place. Net::SCP runs outside mina's command queue, hence
# the explicit --simulate check.
desc "Uploads dist/index.html, dist/hsbt.css, dist/slide, slide/*.pdf and dist/og/*.jpg"
task :"www:upload" => :"www:generate" do
  www = "/home/#{fetch(:user)}/www/hsbt.org"
  files = {
    "dist/index.html" => "#{www}/index.html",
    "dist/hsbt.css" => "#{www}/stylesheets/hsbt.css",
  }
  assets = Dir.glob("slide/*.pdf") + Dir.glob("dist/og/*.jpg")
  if fetch(:simulate)
    files.each { |src, dest| puts "# scp #{src} #{fetch(:domain)}:#{dest}" }
    puts "# scp -r dist/slide #{fetch(:domain)}:#{www}/"
    puts "# scp slide/*.pdf and dist/og/*.jpg that differ from #{fetch(:domain)}:#{www}/slide/"
  else
    require "net/scp"
    require "digest"
    Net::SCP.start(fetch(:domain), fetch(:user)) do |scp|
      files.each { |src, dest| scp.upload!(src, dest) }
      scp.upload!("dist/slide", www, recursive: true)
      # The PDFs and their preview images add up to hundreds of megabytes, so
      # only those whose contents differ from the server's copy are sent.
      remote = scp.session.exec!("cd #{www}/slide && sha256sum -- *.pdf *.jpg 2>/dev/null").to_s
        .lines.to_h { |line| line.chomp.split("  ", 2).reverse }
      assets.each do |asset|
        name = File.basename(asset)
        next if remote[name] == Digest::SHA256.file(asset).hexdigest
        scp.upload!(asset, "#{www}/slide/#{name}")
      end
    end
  end
end

task :deploy do
  invoke :"www:upload"

  deploy do
    invoke :"git:clone"
    invoke :"deploy:cleanup"

    on :launch do
      gemfile_local_gems = %w[
        tdiary-style-gfm
        logger
        fcgi
        holiday_japan
        rss
      ]
      command "printf \"gem '%s'\\n\" #{gemfile_local_gems.join(" ")} > #{fetch(:current_path)}/Gemfile.local"
      # hsbt plugins are placed into shared/hsbt-plugin by mitamae
      # (recipes/default.rb), not pulled from a git checkout here.
      %w[tdiary-contrib].each do |dir|
        command "cd #{fetch(:shared_path)}/#{dir}; git pull --rebase"
      end
      command "ln -s /home/#{fetch(:user)}/www/tdiary.conf #{fetch(:current_path)}/tdiary.conf"

      in_path(fetch(:current_path)) do
        command "sed -i \"/gem 'tdiary-style-gfm'/d\" Gemfile"
        command "#{fetch(:bundle_bin)} config set without '#{fetch(:bundle_withouts)}'"
        invoke :"bundle:install"
        command "chmod 666 Gemfile.lock"
        command "sudo systemctl restart tdiary"
        command "sudo systemctl restart h2o"
      end
    end
  end
end
