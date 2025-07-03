require "pathname"

HOME = Pathname.new("~")

task :default do
  dotfiles =[]
  %w(history public private).each do |path|
    dotfiles += Pathname.glob("#{path}.dotfiles/dot.[a-zA-Z0-9]*")
  end

  dotfiles.each do |file|
    dist = HOME.expand_path.join(file.basename.sub(/dot/, ""))
    dist.unlink if dist.exist?
    dist.make_symlink(file.expand_path)
  end

  path = Pathname.new("dot.zsh.d")
  dist = HOME.expand_path.join(path.basename.sub(/dot/, ""))
  dist.unlink if dist.exist?
  dist.make_symlink(path.expand_path)
end
