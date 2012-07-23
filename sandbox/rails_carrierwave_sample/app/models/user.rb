class User < ActiveRecord::Base
  attr_accessible :name, :avatar
  mount_uploader :avatar, AvatarUploader
end
