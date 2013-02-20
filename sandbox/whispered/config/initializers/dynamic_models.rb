Settings.inquiries_table_names.each do |name|
  class_name = name.singularize.classify
  self.class.const_set class_name, Class.new(ActiveRecord::Base)
  class_name.constantize.table_name = name

  class_name.constantize.class_eval {
    text_columns = columns.select{|c| [:string, :text].include? c.type}.map{|c| c.name.to_sym}

    attr_accessible *text_columns if Rails.env.development?
    searchable do
      text *text_columns
    end
  }
end if Settings.inquiries_table_names.present?
