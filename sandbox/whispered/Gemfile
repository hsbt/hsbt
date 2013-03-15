source 'https://rubygems.org'

gem 'rails', '~> 4.0.0.beta1'

gem 'activerecord-nulldb-adapter'
gem 'mysql2'
# gem 'pg'

gem 'puma', '~> 2.0.0.b6'
gem 'slim-rails'
gem 'jquery-rails'
gem 'kaminari'
gem 'kaminari-bootstrap'

gem 'omniauth-github'
gem 'rails_config'

gem 'sunspot_rails'
gem 'progress_bar'
gem 'system_timer' if RUBY_VERSION < '1.9'

group :assets do
  gem 'bootstrap-sass'
  gem 'font-awesome-sass-rails'
  gem 'sass-rails', '~> 4.0.0.beta1'
  gem 'coffee-rails', '~> 4.0.0.beta1'
  gem 'therubyracer'
  gem 'uglifier'
end

group :development, :test do
  gem 'pit', :require => false
  gem 'capistrano', :require => false
  gem 'capistrano-puma', :require => false
  gem 'capistrano_colors', :require => false

  gem 'sunspot_solr'
  gem 'pry-rails'
  gem 'spring'

  gem 'forgery'
  gem 'factory_girl_rails'
end
