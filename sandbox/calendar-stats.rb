require "bundler/inline"

gemfiles do
  source "https://rubygems.org"
  gem "activesupport"
  gem "google-api-client"
  gem "redis"
  gem "googleauth"
end

require "googleauth"
require "google/apis/calendar_v3"

OOB_URI = "urn:ietf:wg:oauth:2.0:oob"
USER_ID = "" # Your Google account

ROOM_EMAILS = {
  # room: "room account address",
  # ...
}

ROOM_NAMES = {
  # room: "room name",
  # ...
}

def calendar_client
  scope = "https://www.googleapis.com/auth/calendar"
  client_id = ::Google::Auth::ClientId.from_file("client_id.json")
  token_store = if ENV["REDIS_URL"]
    require "googleauth/stores/redis_token_store"
    ::Google::Auth::Stores::RedisTokenStore.new(redis: ENV["REDIS_URL"])
  else
    require "googleauth/stores/file_token_store"
    ::Google::Auth::Stores::FileTokenStore.new(file: "tokens.yaml")
  end
  authorizer = ::Google::Auth::UserAuthorizer.new(client_id, scope, token_store)

  credentials = authorizer.get_credentials(USER_ID)
  if credentials.nil?
    url = authorizer.get_authorization_url(base_url: OOB_URI )
    puts "Open #{url} in your browser and enter the resulting code:"
    code = gets
    credentials = authorizer.get_and_store_credentials_from_code(
    user_id: USER_ID, code: code, base_url: OOB_URI)
  end

  c = ::Google::Apis::CalendarV3::CalendarService.new
  c.client_options.application_name = "cal-stats"
  c.authorization = credentials

  c
end

c = calendar_client

require 'active_support/all'

target = Time.now
intermediate = {}
events = []

ARGV[0].to_i.upto(ARGV[1].to_i) do |i|
  start = i.months.ago.beginning_of_month.iso8601
  finish = i.month.ago.end_of_month.iso8601
  p [start, finish]
  events += ROOM_EMAILS.map do |k,v|
    {k => calendar_client.list_events(v, time_min: start, time_max: finish)}
  end
end

dedup = []

events.each do |v|
  total = 0
  intermediate[v.keys[0]] ||= {}
  v.values[0].items.each do |a|
    next if dedup.include?(a.id)
    next if a.recurrence # 繰り返しの起点となるイベントはカウントしない、なぜか出てくる
    next if a.summary.nil? || a.end.date_time.nil? || a.start.date_time.nil?
    next if (a.start.date_time.hour < 9) || (a.start.date_time.hour == 13) # 始業前と昼休み開始は除外
    next if (a.end.date_time.hour > 19) # 終了が定時後は除外
    next if (a.start.date_time.wday == 0) || (a.start.date_time.wday == 6) # 土日のイベントは除外

    time = (a.end.date_time.to_time - a.start.date_time.to_time)/(60.0 * 60.0)

    next if time > 8 # 営業時間の部屋全てを貸切の場合は除外する

    attendees = a.attendees.select do |attendee|
      !(attendee.email =~ /resource.calendar.google.com/)
    end.map{|attendee| attendee.email }

    intermediate[v.keys[0]][a.id] = {time: time.round(2), summary: a.summary, attendees: attendees, start_time: a.start.date_time.to_time, end_time: a.end.date_time.to_time}

    dedup << a.id
  end
end

results = {}

intermediate.each do |room, events|
  results[room] = {
    time: events.values.inject(0){|s,a| s + a[:time]},
    events: events.count,
    human_time: events.values.inject(0){|s,a| s + a[:time] * a[:attendees].count},
    attendees: events.values.inject(0){|s,a| s + a[:attendees].count},
  }
  results[room][:avg_time] = results[room][:time].to_f/results[room][:events]
  results[room][:avg_attendees] = results[room][:attendees].to_f/results[room][:events]
end

File.open('mtg.csv', "w") do |f|
  f.puts ",time,events,human_time,attendees,avg_time,avg_attendees"
  results.each do |k, v|
    f.puts "#{k},#{v[:time]},#{v[:events]},#{v[:human_time]},#{v[:attendees]},#{v[:avg_time]},#{v[:avg_attendees]}"
  end
end

pp results

events = {}

intermediate.each do |_, room_events|
  room_events.each do |k, v|
    events[k] ||= {}
    events[k][:time] ||= v[:time]
    events[k][:summary] ||= v[:summary]
    events[k][:attendees] ? events[k][:attendees] += v[:attendees] : events[k][:attendees] = v[:attendees]
  end
end

top_human = {}

events.each do |_, v|
  v[:attendees].each do |human|
    p v if human == ARGV[2]
    top_human[human] ||= 0
    top_human[human] += v[:time]
  end
end

top_human.sort_by {|_, v| v }.reverse.each do |k, v|
  p [k, v.round(2)]
end

# top_time = {}
# top_human.each do |k, v|
#   top_time[v.to_s] ||= []
#   top_time[v.to_s] << k
# end
# pp top_time.sort_by {|k, _| k.to_i }.reverse
