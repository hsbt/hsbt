class DeleteOldStatuses
  def initialize(options = {})
    @max_age = options["max_age"]
    @max_age ||= 30
  end

  def run
    ids = Status.where("created_at < ?", @max_age.days.ago)
    if ids.size > 0
      Status.destroy(ids)
      puts "#{ids.size} statuses have been deleted!"
    else
      puts "No statuses have been deleted."
    end
  end
end
