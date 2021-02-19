require 'sass/css'
module Css2sass
  class Convert
    def initialize(css)
      @css = css
    end

    def to_scss
      begin
        Sass::CSS.new(@css).render(:scss)
      rescue Sass::SyntaxError => e
        @error = e
      end
    end
  end
end

Dir.glob('**/*.css') do |css|
  scss = Css2sass::Convert.new(css).to_scss
  File.open("#{css}.scss", "w+") do |f|
    f.write scss
  end
end
