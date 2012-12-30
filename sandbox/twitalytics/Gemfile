source 'http://rubygems.org'

gem 'rails', '3.2.3'

# Gems used only for assets and not required
# in production environments by default.
group :assets do
  gem 'sass-rails',   '~> 3.2.3'
  gem 'coffee-rails', '~> 3.2.1'
  gem 'uglifier', '>= 1.0.3'
end

gem 'jquery-rails'
gem 'json'
gem 'twitter', '2.0.2'

# START:therubyracer
platform :ruby do
  gem 'therubyracer'
end
# END:therubyracer

group :test do
  gem 'rspec-rails'
end

# START:db_driver
group :production do
  # START:pg
  gem 'pg'
  # END:pg
end

group :development, :test do
  # START:sqlite
  gem 'sqlite3'
  # END:sqlite
end
# END:db_driver
