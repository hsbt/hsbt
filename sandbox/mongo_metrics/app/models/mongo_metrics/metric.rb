require "csv"

module MongoMetrics
  class Metric
    include Mongoid::Document

    field :name, type: String
    field :duration, type: Integer
    field :instrumenter_id, type: String
    field :payload, type: Hash
    field :started_at, type: DateTime
    field :created_at, type: DateTime

    def self.store!(args)
      metric = new metric.parse(args)
      metric.save!
    end

    def to_csv
      [name, started_at, duration, instrumenter_id, created_at].to_csv
    end

    def parse(args)
      self.name = args[0]
      self.started_at = args[1]
      self.duration = (args[2] - args[1]) * 1000000
      self.instrumenter_id = args[3]
      self.payload = args[4]
      self.created_at = Time.now.utc
    end
  end
end
