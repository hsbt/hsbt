Settings.inquiries_tables.each do |table|
  class_name = table.name.singularize.classify
  self.class.const_set class_name, Class.new(ActiveRecord::Base)
  class_name.constantize.table_name = table.name

  class_name.constantize.class_eval <<-RUBY
    def self.text_columns
      columns.select{|c| [:string, :text].include? c.type}.map{|c| c.name.to_sym}
    end

    def self.display_columns
      Settings.inquiries_tables.detect{|table| table.name == self.table_name}.display_columns.map(&:to_sym)
    end

    attr_accessible *text_columns if Rails.env.development?
    searchable do
      text *text_columns
    end
  RUBY
end if Settings.inquiries_tables.present?
