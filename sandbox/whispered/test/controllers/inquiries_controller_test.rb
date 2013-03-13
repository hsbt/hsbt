require 'test_helper'

class InquiriesControllerTest < ActionController::TestCase
  setup do
    @inquiry = create(:inquiry)
  end

  test "should not get index without login" do
    get :index
    assert_response :redirect
    assert_redirected_to root_path
  end

  test "should not get search without login" do
    get :search, :q => "example"
    assert_response :redirect
    assert_redirected_to root_path
  end

  test "should not get show inquiry without login" do
    get :show, :id => @inquiry
    assert_response :redirect
    assert_redirected_to root_path
  end
end
