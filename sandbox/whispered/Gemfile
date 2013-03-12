source 'https://rubygems.org'

gem 'rails', '~> 4.0.0.beta1'
gem 'protected_attributes'

gem 'activerecord-nulldb-adapter'
gem 'mysql2'
# gem 'pg'

gem 'slim-rails'
gem 'bootstrap-sass'
gem 'jquery-rails'
gem 'kaminari'

gem 'omniauth-github'
gem 'rails_config'

gem 'sunspot_rails'
gem 'progress_bar'
gem 'system_timer' if RUBY_VERSION < '1.9'

group :assets do
  gem 'sass-rails', '~> 4.0.0.beta1'
  gem 'coffee-rails', '~> 4.0.0.beta1'
  gem 'therubyracer'
  gem 'uglifier'
end

group :development, :test do
  gem 'sunspot_solr'
  gem 'spring'
  gem 'pry-rails'
  gem 'forgery'
  gem 'factory_girl_rails'
end

group :production do
  gem 'puma'
end
