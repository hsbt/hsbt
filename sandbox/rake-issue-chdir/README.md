# Ruby 2.3.0

```shell
% ruby rake_issue.rb
["$RAKE_TEST_SH", "someval"]
[true, #<Process::Status: pid 24928 exit 0>]
["$RAKE_TEST_SH", "someval"]
[true, #<Process::Status: pid 24929 exit 0>]
```

# JRuby 9.0.5.0

```shell
% ruby rake_issue.rb
["$RAKE_TEST_SH", "someval"]
[true, #<Process::Status: pid 21844 exit 0>]
["someval", "someval"]
[false, #<Process::Status: pid 21845 exit 1>]
```

# JRuby 1.7.24

```shell
% ruby rake_issue.rb
["$RAKE_TEST_SH", "someval"]
[true, #<Process::Status: pid=22397,exited(0)>]
["$RAKE_TEST_SH", "someval"]
[true, #<Process::Status: pid=22398,exited(0)>]
```
