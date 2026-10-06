# The nightly backup used to be a line in ubuntu's crontab. Drop it in the
# same run that installs the cron.d entry so the two never write
# www.data.tgz at the same time.
old_entry = "0 3 * * * tar czf ~/backup/www.data.tgz ~/www && s3cmd sync ~/backup s3://hsbt-org-backup"
execute "remove the backup entry from ubuntu's crontab" do
  command "crontab -u ubuntu -l | grep -vxF '#{old_entry}' | crontab -u ubuntu -"
  only_if "crontab -u ubuntu -l | grep -qxF '#{old_entry}'"
end

remote_file "/etc/cron.d/hsbt-org-backup" do
  source "files/etc/cron.d/hsbt-org-backup"
  owner "root"
  group "root"
  mode "644"
end
