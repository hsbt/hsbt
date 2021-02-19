require "test_helper"

class FlashTest < ActionController::TestCase
  tests UsersController

  test "sets notice message on successful creation" do
    post :create, user: { name: "John Doe" }
    assert_equal "User was successfully created.", flash[:notice]
  end

  test "sets notice message on successful update" do
    user = User.create!(name: "John Doe")
    put :update, id: user.id, user: { name: "Another John Doe" }
    assert_equal "User was successfully updated.", flash[:notice]
  end

  test "sets notice message on successful destroy" do
    user = User.create!(name: "John Doe")
    delete :destroy, id: user.id
    assert_equal "User was successfully destroyed.", flash[:notice]
  end

  test "sets alert messages from the controller scope" do
    begin
      I18n.backend.store_translations :en, flash: { users: { destroy: { alert: "Cannot destroy!" } } }
      user = User.create!(name: "Undestroyable")
      delete :destroy, id: user.id
      assert_equal "Cannot destroy!", flash[:alert]
    ensure
      I18n.reload!
    end
  end
end
