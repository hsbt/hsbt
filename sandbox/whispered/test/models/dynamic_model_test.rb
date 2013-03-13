require 'test_helper'

class DynamicModelTest < ActiveSupport::TestCase
  test 'self.all' do
    assert_equal [Inquiry], DynamicModel.all
  end
end
