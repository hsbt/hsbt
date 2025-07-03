require "clockwork"
require "heroku"

include Clockwork
client = Heroku::Client.new(*Heroku::Auth.read_credentials)

handler do |job|
  case job
  when "dynos.up"
    client.ps_scale("myapplication", type: :web, qty: 2)
  when "dynos.down"
    client.ps_scale("myapplication", type: :web, qty: 1)
  end
end

every(1.days, "dynos.up", at: "10:00")
every(1.days, "dynos.down", at: "22:00")
