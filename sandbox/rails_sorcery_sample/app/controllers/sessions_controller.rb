class SessionsController < ApplicationController
  skip_before_filter :require_login

  def create
    respond_to do |format|
      if @user = login(params[:session][:username], params[:session][:password])
        format.html { redirect_to(:users) }
      else
        format.html { render :action => "new" }
      end
    end
  end
end
