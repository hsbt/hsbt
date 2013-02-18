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
    @inquiries = Inquiry.search do
      fulltext params[:q]
      paginate :page => (params[:page] || 1), :per_page => 10
    end.results
  end

  def show
    @inquiry = Inquiry.find(params[:id])
  end
end
