require "geminabox"

Geminabox.data = File.expand_path("~/.geminabox-data", __FILE__)

run Geminabox::Server
