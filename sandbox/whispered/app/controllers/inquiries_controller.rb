class InquiriesController < ApplicationController
  before_filter :login_required, :except => [:index]

  respond_to :html

  def index
    @inquiries = []
    Settings.inquiries_tables.each do |table|
      class_name = table.name.singularize.classify
      @inquiries << class_name.constantize.all(:limit => 5)
    end
    @inquiries.flatten!
  end

  def search
    @inquiries = []
    Settings.inquiries_tables.each do |table|
      class_name = table.name.singularize.classify
      @inquiries << class_name.constantize.search do
        fulltext params[:q]
        paginate :page => 1, :per_page => 250
      end.results
    end
    @inquiries.flatten!
    @inquiries = Kaminari.paginate_array(@inquiries).page(params[:page]).per(25)
  end

  def show
    @inquiry = params[:class_name].constantize.find(params[:id])
  end
end
