class Inquiry < ActiveRecord::Base
  attr_accessible :email, :inq_type, :message, :name
end
