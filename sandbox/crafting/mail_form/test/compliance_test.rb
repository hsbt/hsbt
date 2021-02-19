require 'test_helper'
require 'fixtures/sample_mail'

class ComplianceTest < ActiveSupport::TestCase
  include ActiveModel::Lint::Tests

  def setup
    @model = SampleMail.new
  end

  test "model_name exposes singular and human name" do
    assert_equal "sample_mail", @model.class.model_name.singular
    assert_equal "Sample mail", @model.class.model_name.human
  end

  test "model_name.human uses I18n" do
    begin
      I18n.backend.store_translations :en, activemodel: { models: { sample_mail: "My Sample Mail" } }
      assert_equal "My Sample Mail", @model.class.model_name.human
    ensure
      I18n.reload!
    end
  end  
end
