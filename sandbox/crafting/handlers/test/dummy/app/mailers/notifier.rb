class Notifier < ActionMailer::Base
  def contact(recipient)
    @recipient = recipient
    mail(to: @recipient, from: "john.doe@example.com") do |format|
      format.text
      format.html
    end
  end
end
