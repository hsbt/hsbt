#---
# Excerpted from "Deploying with JRuby",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material, 
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose. 
# Visit http://www.pragmaticprogrammer.com/titles/jkdepj for more book information.
#---
module TwitterUtil
  def fetch_recent_tweets(num)
    with_twitter_stream_config do |keywords|
      Twitter.search(
        keywords.join(" "), {:result_type=>"recent", :rpp=>num}
      ).each {|t|}
    end
  end

  def fetch_tweets_since(since_id)
    with_twitter_stream_config do |keywords|
      if since_id
        results = Twitter.search(
            keywords.join(" "), {:result_type=>"recent", :since_id=>"since_id"}
        ).each {|t|}
      else
        results = Twitter.search(
            keywords.join(" "), {:result_type=>"recent", :per_page=>20}
        ).each {|t|}
      end
      results.sort{|x,y| x["created_at"] <=> y["created_at"]}.each {|r| yield r }
      results
    end
  end

  def with_twitter_account
    cnfg = YAML.load_file("#{Rails.root}/config/twitter.yml")
    yield cnfg['username']
  end

  def with_twitter_stream_config
    cnfg = YAML.load_file("#{Rails.root}/config/twitter.yml")
    yield cnfg['keywords']
  end
end
