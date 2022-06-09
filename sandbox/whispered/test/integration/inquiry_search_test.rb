require 'test_helper'
 
class InquirySearchTest < ActionDispatch::IntegrationTest
  test "paginate inquiry" do
    11.times { create(:inquiry) }
    post_via_redirect "/auth/github"

    get "/inquiries/search", :q => "lorem"
    assert_response :success
    assert_match /next/, @response.body
  end

  test "show inquiry" do
    @inquiry = create(:inquiry)
    post_via_redirect "/auth/github"

    get "/inquiries", {:id => @inquiry.id, :class_name => "Inquiry"}
    assert_response :success
    assert_match /lorem/, @response.body
  end
end
