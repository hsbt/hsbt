# -*- coding: utf-8 -*-
FactoryGirl.define do
  factory :inquiry do
    name { Forgery::Name.full_name }
    email { Forgery(:internet).email_address }
    message { Forgery(:lorem_ipsum).words(100) }
  end
end
