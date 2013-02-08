class InquiriesController < ApplicationController
  before_filter :login_required, :except => [:index]

  respond_to :html

  def index
    @inquiries = Inquiry.all(:limit => 5)
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
