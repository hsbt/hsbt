def sh(*cmd)
  res = system(*cmd)
  status = $?
  p [res, status]
end

RUBY = ENV["RUBY"] || File.join(
    RbConfig::CONFIG["bindir"],
    RbConfig::CONFIG["ruby_install_name"] + RbConfig::CONFIG["EXEEXT"])

ENV["RAKE_TEST_SH"] = "someval"

sh RUBY, "check_no_expansion.rb", "$RAKE_TEST_SH", "someval"

Dir.chdir "test"
sh RUBY, "check_no_expansion.rb", "$RAKE_TEST_SH", "someval"
