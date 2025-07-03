class SampleMail < MailForm::Base
  attributes :name, :email

  def headers
    { to: "recipient@example.com", from: self.email }
  end
end
