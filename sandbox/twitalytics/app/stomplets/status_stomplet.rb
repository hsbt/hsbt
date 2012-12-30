class StatusStomplet < TorqueBox::Stomp::JmsStomplet
  def on_subscribe(subscriber)
    topic = destination_for('/topics/statuses', :topic)
    subscribe_to(subscriber, topic)
  end
end
