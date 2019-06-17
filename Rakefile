require "rake"
require "fileutils"

task :push do
    files = %w[fishfile config.fish conf.d/000-env.fish conf.d/zzz-env.fish]
    files.each do |f|
        from = File.join("xdg_config", "fish", f)
        to = File.join(ENV['XDG_CONFIG_HOME'], "fish", f)
        FileUtils.cp from, to
    end
end

task :pull do
    files = %w[fishfile config.fish conf.d/000-env.fish conf.d/zzz-env.fish]
    files.each do |f|
        from = File.join(ENV['XDG_CONFIG_HOME'], "fish", f)
        to = File.join("xdg_config", "fish", f)
        FileUtils.cp from, to
    end
end
