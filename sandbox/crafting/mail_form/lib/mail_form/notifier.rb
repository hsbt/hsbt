module MailForm
  class Notifier < ActionMailer::Base
    append_view_path File.expand_path("../../views", __FILE__)

    def contact(mail_form)
      @mail_form = mail_form
      mail(mail_form.headers)
    end
  end
end
