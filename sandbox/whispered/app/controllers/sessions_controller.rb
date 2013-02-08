class SessionsController < ApplicationController
  def create
    self.current_user = auth_hash[:info]
    redirect_to search_inquiries_path
  end

  def destroy
    session[:current_user] = nil
    redirect_to '/', :notice => "Signed out"
  end

  protected

  def auth_hash
    request.env['omniauth.auth']
  end
end
