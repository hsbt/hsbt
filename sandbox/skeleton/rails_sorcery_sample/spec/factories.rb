FactoryGirl.define do
  factory :user do
    sequence(:username) { |n| "Person #{n}" }
    sequence(:email) { |n| "person_#{n}@exqmple.com" }
    password { "foobar" }
  end
end
