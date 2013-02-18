class InquiriesController < ApplicationController
  before_filter :login_required, :except => [:index]

  respond_to :html

  def index
    @inquiries = []
    Settings.inquiries_table_names.each do |name|
      class_name = name.singularize.classify
      @inquiries << class_name.constantize.all(:limit => 5)
    end
    @inquiries.flatten!
  end

  def search
    @inquiries = []
    Settings.inquiries_table_names.each do |name|
      class_name = name.singularize.classify
      @inquiries << class_name.constantize.search do
        fulltext params[:q]
      end.results
    end
    @inquiries.flatten!
    @inquiries = Kaminari.paginate_array(@inquiries).page(params[:page]).per(10)
  end

  def show
    @inquiry = Inquiry.find(params[:id])
  end
end
