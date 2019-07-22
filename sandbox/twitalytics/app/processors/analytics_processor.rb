class AnalyticsProcessor < TorqueBox::Messaging::MessageProcessor
  def on_message(body)
    statuses = JSON.parse(body).map do |s|
      status = Status.find(s['id'])
      status.preprocess!
      status
    end
    Analytics.refresh(statuses)
  end
end
