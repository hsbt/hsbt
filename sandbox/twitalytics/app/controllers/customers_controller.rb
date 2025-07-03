class CustomersController < ApplicationController
  def index
    @statuses = Status.find(:all, order: "created_at desc", limit: 20)
  end

  def retweet
    Status.find(params[:id]).background.retweet
    redirect_to customers_path
  end
end
