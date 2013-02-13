class Inquiry < ActiveRecord::Base
  table_name = Settings.inqueries_table_name

  searchable do
    text *columns.select{|c| [:string, :text].include? c.type}.map{|c| c.name.to_sym}
  end
end
