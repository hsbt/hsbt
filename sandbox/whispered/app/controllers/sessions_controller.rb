class SessionsController < ApplicationController
  def create
    self.current_user = request.env['omniauth.auth'][:info]
    redirect_to search_inquiries_path
  end

  def destroy
    session[:current_user] = nil
    redirect_to '/', :notice => "Signed out"
  end
end
