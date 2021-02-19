class User < ActiveRecord::Base
  before_destroy do
    if name == "Undestroyable"
      errors.add(:base, "is undestroyable")
      false
    end
  end
end
