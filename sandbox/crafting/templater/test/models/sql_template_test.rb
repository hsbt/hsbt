# -*- coding: utf-8 -*-
require 'test_helper'

class SqlTemplateTest < ActiveSupport::TestCase
  test "resolver returns a template with the saved body" do
    resolver = SqlTemplate::Resolver.new
    details  = { formats: [:html], locale: [:en], handlers: [:erb] }
    # 1) Assert our resolver cannot find any template as the database is empty
    # find_all(name, prefix, partial, details)
    assert resolver.find_all("index", "posts", false, details).empty?

    # 2) Create a template in the database
    SqlTemplate.create!(
      body: "<%= 'Hi from SqlTemplate!' %>", path: "posts/index",
      format: "html", locale: "en", handler: "erb", partial: false)

    # 3) Assert that a template can now be found
    template = resolver.find_all("index", "posts", false, details).first
    assert_kind_of ActionView::Template, template

    # 4) Assert specific information about the found template
    assert_equal "<%= 'Hi from SqlTemplate!' %>", template.source
    assert_kind_of ActionView::Template::Handlers::ERB, template.handler
    assert_equal [:html], template.formats
    assert_equal "posts/index", template.virtual_path
    assert_match %r[SqlTemplate - \d+ - "posts/index"], template.identifier
  end
end
