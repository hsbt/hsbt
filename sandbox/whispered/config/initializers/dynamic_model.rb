Settings.inquiries_tables.each do |table|
  class_name = table.name.singularize.classify
  self.class.const_set class_name, Class.new(ActiveRecord::Base)
  class_name.constantize.table_name = table.name

  class_name.constantize.class_eval do |klass|
    def klass.text_columns
      columns.select{|c| [:string, :text].include?(c.type) && c.name != settings.created_at }.map{|c| c.name.to_sym}
    end

    def klass.display_columns
      settings.display_columns.map(&:to_sym)
    end

    def klass.settings
      Settings.inquiries_tables.detect{|table| table.name == self.table_name}
    end

    def created_at
      Time.parse(send(self.class.settings.created_at.to_sym))
    end if table.created_at && !respond_to?(:created_at)

    attr_accessible *text_columns if Rails.env.development?
    searchable do
      text *text_columns
      time :created_at if settings.created_at
    end
  end
end if Settings.inquiries_tables.present? && !ENV['TRAVIS_INIT']

module DynamicModel
  def all
    Settings.inquiries_tables.map do |table|
      table.name.singularize.classify.constantize
    end
  end
  module_function :all
end
