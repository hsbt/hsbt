class ApplicationController < ActionController::Base
  protect_from_forgery
  helper_method :current_user

  private

  def login_required
    redirect_to root_path unless session[:current_user]
  end

  def current_user
    session[:current_user]
  end

  def current_user=(user)
    session[:current_user] = user
  end
end
