require "test_helper"

class PdfDeliveryTest < ActionDispatch::IntegrationTest
  test "pdf request sends a pdf as file" do
    get home_path(format: :pdf)
    assert_match "PDF", response.body
    assert_equal "binary", headers["Content-Transfer-Encoding"]
    assert_equal "attachment; filename=\"contents.pdf\"", headers["Content-Disposition"]
    assert_equal "application/pdf", headers["Content-Type"]
  end
end
