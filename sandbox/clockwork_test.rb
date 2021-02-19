require 'clockwork'
require 'mail'
include Clockwork

Mail.defaults do
  delivery_method :smtp, :user_name => '',
                         :password  => '',
                         :address   => 'smtp.gmail.com',
                         :port      => 587
end

handler do |job|
  case job
  when 'alarm.job'
    Mail.deliver do
      from ''
      to ''
      subject "Alarm"
      body 'Awesome!'
    end
  end
end

every(3.minutes, 'alarm.job', at: '09:00')
