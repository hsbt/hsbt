TorqueBox.configure do
  pool :web, :type => :shared
  job DeleteOldStatuses do
    cron "0 0/5 * * * ?"
    config do
      max_age 30
    end
  end
end
