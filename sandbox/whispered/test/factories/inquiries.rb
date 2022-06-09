# -*- coding: utf-8 -*-
FactoryGirl.define do
  factory :inquiry do
    name { Forgery::Name.full_name }
    email { Forgery(:internet).email_address }
    message { Forgery(:lorem_ipsum).words(20) }
    create_date { Time.now.to_s }
    update_date { Time.now.to_s }
  end
end
