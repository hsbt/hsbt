Rails.application.routes.draw do
  get "/live_assets/:action", to: "live_assets"
end
