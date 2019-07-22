class UpdateAnalytics
  @queue = :normal

  def self.perform(status_ids)
    puts "The analytics worker is running"
    statuses = status_ids.map  do |status_id|
      status = Status.find(status_id)
      status.preprocess!
      status
    end
    Analytics.refresh(statuses)
  end
end
