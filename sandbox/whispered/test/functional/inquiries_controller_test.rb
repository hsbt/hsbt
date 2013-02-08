require 'test_helper'

class InquiriesControllerTest < ActionController::TestCase
  setup do
    @inquiry = create(:inquiry)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:inquiries)
  end

  test "should get search" do
    get :search, :q => "example"
    assert_response :success
    assert_match /example/, @response.body
  end

  test "should get search with paginate" do
    11.times { create(:inquiry) }

    get :search, :q => "example"
    assert_response :success
    assert_match /next/, @response.body
  end

  test "should not get new" do
    assert_raise(ActionController::RoutingError) do
      get :new
    end
  end

  test "should not create inquiry" do
    assert_raise(ActionController::RoutingError) do
      post :create, :inquiry => { :email => @inquiry.email, :inq_type => @inquiry.inq_type, :message => @inquiry.message, :name => @inquiry.name }
    end
  end

  test "should show inquiry" do
    get :show, :id => @inquiry
    assert_response :success
  end

  test "should not get edit" do
    assert_raise(ActionController::RoutingError) do
      get :edit, :id => @inquiry
    end
  end

  test "should not update inquiry" do
    assert_raise(ActionController::RoutingError) do
      put :update, :id => @inquiry, :inquiry => { :email => @inquiry.email, :inq_type => @inquiry.inq_type, :message => @inquiry.message, :name => @inquiry.name }
    end
  end

  test "should not destroy inquiry" do
    assert_raise(ActionController::RoutingError) do
      delete :destroy, :id => @inquiry
    end
  end
end
