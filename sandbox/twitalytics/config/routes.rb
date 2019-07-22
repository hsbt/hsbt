require 'resque/server'

Twitalytics::Application.routes.draw do
  get "customers/index", :as => :customers

  get "company/index", :as => :company

  post "company/update", :as => :company_status

  get "dashboard/index"

  post "customers/retweet/:id", :controller => :customers, :action => :retweet, :as => :retweet

  root :to => "dashboard#index"

  mount Resque::Server.new, :at => "/resque"
end
