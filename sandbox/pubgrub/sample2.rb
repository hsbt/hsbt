require "pub_grub"
require "pub_grub/rubygems"

source = PubGrub::StaticPackageSource.new do |s|
  s.add "foo", "2.0.0", deps: { "bar" => "1.0.0" }
  s.add "foo", "1.0.0"

  s.add "bar", "1.0.0", deps: { "foo" => "1.0.0" }

  s.root deps: { "bar" => ">= 1.0.0" }
end

solver = PubGrub::VersionSolver.new(source: source)
result = solver.solve
p result
#=> {#<PubGrub::Package :root>=>0, "bar"=>#<Gem::Version "1.0.0">, "foo"=>#<Gem::Version "1.0.0">}
