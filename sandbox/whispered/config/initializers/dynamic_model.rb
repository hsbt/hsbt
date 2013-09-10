Settings.inquiries_tables.each do |table|
  class_name = table.name.singularize.classify
  self.class.const_set class_name, Class.new(ActiveRecord::Base)

  class_name.constantize.class_eval do |klass|
    klass.establish_connection table.database.to_hash
    klass.table_name = table.name

    klass.send(:include, Tire::Model::Search)
    klass.send(:include, Tire::Model::Callbacks)

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
    rescue
      nil
    end if table.created_at && !respond_to?(:created_at)

    mapping do
      text_columns.each do |attr|
        indexes attr.to_sym, :type => 'string', :index => :not_analyzed
      end
      indexes :created_at, :type => 'date', :index => :not_analyzed if settings.created_at
    end
  end
end if Settings.inquiries_tables.present?

module DynamicModel
  def all
    Settings.inquiries_tables.map do |table|
      table.name.singularize.classify.constantize
    end
  end
  module_function :all
end
