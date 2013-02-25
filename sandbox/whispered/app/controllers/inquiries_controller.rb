class InquiriesController < ApplicationController
  before_filter :login_required

  respond_to :html

  def index
    @inquiries = []
    DynamicModel.all.each do |klass|
      @inquiries << klass.all(:limit => 5)
    end
    @inquiries.flatten!
  end

  def search
    @inquiries = []
    DynamicModel.all.each do |klass|
      @inquiries << klass.search do
        fulltext params[:q]
      end.results
    end
    @inquiries.flatten!
    @inquiries = Kaminari.paginate_array(@inquiries).page(params[:page]).per(10)
  end

  def show
    @inquiry = params[:class_name].constantize.find(params[:id])
  end
end
