require 'carrierwave/orm/activerecord'

CarrierWave.configure do |config|
  config.fog_credentials = {
    :provider               => 'AWS',
    :aws_access_key_id      => Settings.aws.access_key,
    :aws_secret_access_key  => Settings.aws.secret_key,
  }
  config.fog_directory  = 'hsbt-fog'
end
