json.array!(@sql_templates) do |sql_template|
  json.extract! sql_template, :id, :body, :path, :format, :locale, :handler, :partial
  json.url sql_template_url(sql_template, format: :json)
end
