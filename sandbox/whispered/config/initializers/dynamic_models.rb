Settings.inqueries_table_names.each do |name|
  class_name = name.singularize.classify
  self.class.const_set class_name, Class.new(ActiveRecord::Base)
  class_name.constantize.class_eval {
    searchable do
      text *columns.select{|c| [:string, :text].include? c.type}.map{|c| c.name.to_sym}
    end
  }
end
