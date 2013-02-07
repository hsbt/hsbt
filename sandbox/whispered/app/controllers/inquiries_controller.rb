class InquiriesController < ApplicationController
  def index
    @inquiries = Inquiry.all

    respond_to do |format|
      format.html
    end
  end

  def search
    @inquiries = Inquiry.search { fulltext params[:q] }.results

    respond_to do |format|
      format.html
    end
  end

  def show
    @inquiry = Inquiry.find(params[:id])

    respond_to do |format|
      format.html
    end
  end

  def edit
    @inquiry = Inquiry.find(params[:id])
  end

  def update
    @inquiry = Inquiry.find(params[:id])

    respond_to do |format|
      if @inquiry.update_attributes(params[:inquiry])
        format.html { redirect_to @inquiry, :notice => 'Inquiry was successfully updated.' }
      else
        format.html { render :action => "edit" }
      end
    end
  end
end
