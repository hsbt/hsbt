rails_env = ENV['RAILS_ENV'] || 'production'

threads 4,4

bind  "unix:///home/whispered/app/whispered/shared/sockets/puma.sock"
pidfile "/home/whispered/app/whispered/shared/pids/puma.pid"
state_path "/home/whispered/app/whispered/shared/sockets/puma.state"

activate_control_app
