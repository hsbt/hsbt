class Inquiry < ActiveRecord::Base
  attr_accessible :email, :message, :name

  searchable do
    text :email, :name, :message
  end
end
