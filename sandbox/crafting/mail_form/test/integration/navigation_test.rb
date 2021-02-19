# -*- coding: utf-8 -*-
require "test_helper"
class NavigationTest < ActiveSupport::IntegrationCase
  setup do
    ActionMailer::Base.deliveries.clear
  end

  test "sends an e-mail after filling the contact form" do
    visit "/"
    fill_in "Name", with: "John Doe"
    fill_in "Email", with: "john.doe@example.com"
    fill_in "Message", with: "MailForm rocks!"
    click_button "Deliver"

    assert_match "Your message was successfully sent.", page.body
    assert_equal 1, ActionMailer::Base.deliveries.size

    mail = ActionMailer::Base.deliveries.last

    assert_equal ["john.doe@example.com"], mail.from
    assert_equal ["recipient@example.com"], mail.to
    assert_match "Message: MailForm rocks!", mail.body.encoded
  end
end
