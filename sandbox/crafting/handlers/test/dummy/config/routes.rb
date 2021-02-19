Rails.application.routes.draw do
  get "/handlers/:action", to: "handlers"
end
