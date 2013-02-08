Rails.application.config.middleware.use OmniAuth::Builder do
  provider :github, Settings.github.key, Settings.github.secret
end
