Rails.application.routes.draw do

  get 'home/foo'

  get 'home/bar'

  get 'home/baz'

  mount MongoMetrics::Engine => "/mongo_metrics"
end
