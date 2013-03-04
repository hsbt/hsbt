Rails.application.config.middleware.use OmniAuth::Builder do
  provider :github, Settings.github.key, Settings.github.secret
end
OmniAuth.config.full_host = Settings.omni_auth.full_host if Settings.omni_auth.try(:full_host)
