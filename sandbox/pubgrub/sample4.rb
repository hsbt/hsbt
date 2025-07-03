source = PubGrub::StaticPackageSource.new do |s|
  s.add "foo", "3.0.0", deps: { "bar" => "> 1.0.0" }
  s.add "foo", "2.0.0", deps: { "bar" => "1.0.0" }
  s.add "foo", "1.0.0"

  s.add "bar", "1.0.0", deps: { "foo" => "1.0.0" }
  s.add "bar", "2.0.0"

  s.add "buzz", "1.0.0", deps: { "foo" => "> 1.0.0" }

  s.root deps: { "buzz" => "1.0.0" }
end

solver = PubGrub::VersionSolver.new(source: source)
result = solver.solve
p result
#=> {#<PubGrub::Package :root>=>0, "buzz"=>#<Gem::Version "1.0.0">, "foo"=>#<Gem::Version "3.0.0">, "bar"=>#<Gem::Version "2.0.0">}
