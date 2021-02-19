module Responders
  module Generators
    class InstallGenerator < Rails::Generators::Base
      source_root File.expand_path("../templates", __FILE__)
      def copy_template_file
        copy_file "controller.rb", "lib/templates/rails/scaffold_controller/controller.rb"
      end
    end
  end
end
