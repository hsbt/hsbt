TorqueBox.configure do
  pool :web, type: :shared

  job DeleteOldStatuses do
    cron "0 0/5 * * * ?"
    config do
      max_age 30
    end
  end

  service TwitterStreamService

  options_for Backgroundable, concurrency: 10

  topic "/topics/statuses" do
    processor AnalyticsProcessor
  end

  stomp do
    host "localhost"
  end

  stomplet StatusStomplet do
    route "/stomp/status"
  end
end
