require 'test_helper'

class UserLoginTest < ActionDispatch::IntegrationTest
  test "login and browse site" do
    get "/"
    assert_response :success
    assert_match /awesome/, @response.body

    post_via_redirect "/auth/github"
    assert_equal '/', path
    assert_match /hsbt/, @response.body
    assert_no_match /awesome/, @response.body
  end
end
