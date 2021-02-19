require "test_helper"

class NavigationTest < ActionDispatch::IntegrationTest
  setup { MongoMetrics::Metric.delete_all }

  test "can visualize notifications" do
    get main_app.home_foo_path
    get main_app.home_bar_path
    get main_app.home_baz_path
    get mongo_metrics.root_path
    assert_match "Path: /home/foo", response.body
    assert_match "Path: /home/bar", response.body
    assert_match "Path: /home/baz", response.body
  end

  test "can destroy notifications" do
    get main_app.home_foo_path
    metric = MongoMetrics::Metric.first
    delete mongo_metrics.metric_path(metric)
    assert_empty MongoMetrics::Metric.where(id: metric.id)
  end

  test "does not log engine actions" do
    get mongo_metrics.root_path
    assert 0, MongoMetrics::Metric.count
  end

  test "exports data to csv" do
    get main_app.home_foo_path
    get mongo_metrics.metrics_path(format: :csv)
    assert_match "process_action.action_controller,", response.body
  end
end
