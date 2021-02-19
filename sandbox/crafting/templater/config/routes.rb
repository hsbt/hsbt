Rails.application.routes.draw do
  get "cms/*page", to: "cms#respond"

  resources :users

  resources :sql_templates
end
