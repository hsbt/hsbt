class Inquiry < ActiveRecord::Base
  attr_accessible :email, :inq_type, :message, :name

  searchable do
    text :email, :name
    text :message, :stored => true
  end
end
