%w[
REXML_2_7_0
SHELL_v0_6
forwardable_v1_1
gtk_012
irb_0_7_1
irb_0_7_3
irb_0_9
net_1_1_1
net_1_1_3
oniguruma_2_2_4
oniguruma_2_2_5
oniguruma_2_2_6
oniguruma_2_2_7
oniguruma_2_2_8
oniguruma_3_4_0
oniguruma_3_5_4
oniguruma_3_6_0
oniguruma_3_7_0
oniguruma_3_7_0_1
sha1_1_2
testunit_0_1_7
testunit_0_1_8
RUBY_1_3
V1-1-3
V1-1-1
RUBY-SHA1_v1_2
r1_1b9
r1_1b9_24
r1_1b9_25
ruby1_4_3
ruby_1_4_3
ruby_1_4_3_pre1
].each do |tag|
  `git push origin :#{tag}`
end