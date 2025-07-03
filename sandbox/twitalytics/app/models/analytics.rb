require "lib/analytics_util"

class Analytics < ActiveRecord::Base
  def self.refresh(statuses)
    before = statuses.map(&:created_at).max
    statuses = Status.where("created_at < ?", before).order("created_at desc").limit(200)
    Analytics.create(
      positivity_followers_r: calc_positivity_followers_r(statuses),
      positivity_stdv: calc_positivity_stdv(statuses)
    )
  end

  protected

  def self.calc_positivity_followers_r(statuses)
    x = statuses.map(&:positivity_score)
    y = statuses.map(&:followers_count)
    AnalyticsUtil.pearson_r(x, y)
  end

  def self.calc_positivity_stdv(statuses)
    scores = statuses.map(&:positivity_score)
    AnalyticsUtil.standard_dev(scores)
  end
end
