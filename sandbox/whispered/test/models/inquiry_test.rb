require 'test_helper'

class InquiryTest < ActiveSupport::TestCase
  test "self.text_columns" do
    assert_equal Inquiry.text_columns, [:name, :email, :message, :update_date]
  end

  test "self.display_columns" do
    assert_equal Inquiry.display_columns, [:name, :email, :message]
  end
end
