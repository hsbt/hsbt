require "mongo_metrics/mute_middleware"

module MongoMetrics
  class Engine < ::Rails::Engine
    isolate_namespace MongoMetrics
    config.middleware.use MuteMiddleware
  end
end
