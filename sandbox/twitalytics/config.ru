require ::File.expand_path('../config/environment',  __FILE__)
require 'torquebox-stomp'

use TorqueBox::Stomp::StompJavascriptClientProvider

run Twitalytics::Application
