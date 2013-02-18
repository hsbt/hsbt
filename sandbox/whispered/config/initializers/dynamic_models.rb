Settings.inquiries_table_names.each do |name|
  class_name = name.singularize.classify
  self.class.const_set class_name, Class.new(ActiveRecord::Base)
  class_name.constantize.class_eval {
    column_names = columns.select{|c| [:string, :text].include? c.type}.map{|c| c.name.to_sym}

    attr_accessible *column_names if Rails.env.development?
    searchable do
      text *column_names
    end
  }
end if Settings.inquiries_table_names.present?
