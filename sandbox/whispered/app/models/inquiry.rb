class Inquiry < ActiveRecord::Base
  attr_accessible :email, :inq_type, :message, :name

  searchable do
    integer :inq_type
    text :email, :name, :message
  end
end
