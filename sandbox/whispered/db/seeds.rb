10.times do
  Inquiry.create!({
      :name => Forgery::Name.full_name,
      :email => Forgery(:internet).email_address,
      :message => Forgery(:lorem_ipsum).words(10),
      :inq_type => rand(7)
    })
end
