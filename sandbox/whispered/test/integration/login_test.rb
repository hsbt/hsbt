require 'test_helper'

class UserLoginTest < ActionDispatch::IntegrationTest
  test "login and browse site" do
    get "/"
    assert_response :success

    post_via_redirect "/auth/github"
    assert_equal '/inquiries/search', path
    assert_match /hsbt/, @response.body
  end
end
