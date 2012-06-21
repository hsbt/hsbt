class ApplicationController < ActionController::Base
  protect_from_forgery

  before_filter :require_login, :except => [:not_authenticated]

  protected

  def not_authenticated
    redirect_to new_session_path
  end
end
