require 'test_helper'

class HomeControllerTest < ActionController::TestCase
  test "should get foo" do
    get :foo
    assert_response :success
  end

  test "should get bar" do
    get :bar
    assert_response :success
  end

  test "should get baz" do
    get :baz
    assert_response :success
  end

end
