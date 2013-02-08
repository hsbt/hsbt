class InquiriesController < ApplicationController
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

  def update
    @inquiry = Inquiry.find(params[:id])

    if @inquiry.update_attributes(params[:inquiry])
      redirect_to @inquiry, :notice => 'Inquiry was successfully updated.'
    else
      render :action => "edit"
    end
  end
end
