require "rspec"
require "rspec/autorun"

describe "Ruby 2.1 segfault" do
  let(:argument) { double }

  subject { subject.new(double) }

  it "will segfault" do
    subject
  end
end
