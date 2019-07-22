module ApplicationHelper
  include TorqueBox::Injectors

  def stomp_url
    inject('stomp-endpoint')
  end
end
