# -*- coding: utf-8 -*-
require "test_helper"
class RenderingTest < ActionDispatch::IntegrationTest
  test ".rb template handler" do
    get "/handlers/rb_handler"
    expected = "This is my first <b>template handler</b>!"
    assert_match expected, response.body
  end

  test ".string template handler" do
    get "/handlers/string_handler"
    expected = "Congratulations! You just created another template handler!"
    assert_match expected, response.body
  end

  test ".md template handler" do
    get "/handlers/rdiscount"
    expected = "<p>RDiscount is <em>cool</em> and <strong>fast</strong>!</p>"
    assert_match expected, response.body
  end

  test ".merb template handler" do
    get "/handlers/merb"
    expected = "<p>MERB template handler is <strong>cool and fast</strong>!</p>"
    assert_match expected, response.body.strip
  end

  test "dual template with .merb" do
    email = Notifier.contact("you@example.com")
    assert_equal 2, email.parts.size
    assert_equal "multipart/alternative", email.mime_type
    assert_equal "text/plain", email.parts[0].mime_type
    assert_equal "Dual templates **rock**!", email.parts[0].body.encoded.strip
    assert_equal "text/html", email.parts[1].mime_type
    assert_equal "<p>Dual templates <strong>rock</strong>!</p>", email.parts[1].body.encoded.strip
  end
end
