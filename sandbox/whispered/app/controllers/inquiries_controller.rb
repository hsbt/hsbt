class InquiriesController < ApplicationController
  before_filter :login_required

  respond_to :html

  def index
    @inquiries = DynamicModel.all.map{|model| model.limit(5)}.flatten
  end

  def search
    query_string = params[:q]
    @inquiries = DynamicModel.all.map {|model|
      model.search do
        query { string query_string }
        sort { by :created_at, 'desc' } if model.settings.created_at
        size 50 # per_page * 5
      end.results
    }.flatten
    @inquiries = Kaminari.paginate_array(@inquiries).page(params[:page]).per(10)
  end

  def show
    @inquiry = params[:class_name].constantize.find(params[:id])
  end
end
