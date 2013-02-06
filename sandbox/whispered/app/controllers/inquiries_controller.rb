class InquiriesController < ApplicationController
  def index
    @inquiries = Inquiry.all

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

  def new
    @inquiry = Inquiry.new

    respond_to do |format|
      format.html
    end
  end

  def edit
    @inquiry = Inquiry.find(params[:id])
  end

  def create
    @inquiry = Inquiry.new(params[:inquiry])

    respond_to do |format|
      if @inquiry.save
        format.html { redirect_to @inquiry, :notice => 'Inquiry was successfully created.' }
      else
        format.html { render :action => "new" }
      end
    end
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

  def destroy
    @inquiry = Inquiry.find(params[:id])
    @inquiry.destroy

    respond_to do |format|
      format.html { redirect_to inquiries_url }
    end
  end
end
