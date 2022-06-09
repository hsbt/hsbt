class SessionsController < ApplicationController
  def create
    if Settings.members.include? request.env['omniauth.auth'][:info][:nickname]
      self.current_user = request.env['omniauth.auth'][:info]
    else
      flash[:error] = "Access denied, You can't have permissions."
    end
    redirect_to root_path
  end

  def destroy
    session[:current_user] = nil
    redirect_to '/', :notice => "Signed out"
  end
end
