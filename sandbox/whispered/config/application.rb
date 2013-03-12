require File.expand_path('../boot', __FILE__)

require 'rails/all'

# Assets should be precompiled for production (so we don't need the gems loaded then)
Bundler.require(*Rails.groups(assets: %w(development test)))

module Whispered
  class Application < Rails::Application
    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    config.time_zone = 'Tokyo'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de

    config.generators do |g|
      g.fixture_replacement :factory_girl
      g.stylesheets = false
      g.javascripts = false
      g.heler = false
    end
  end
end
