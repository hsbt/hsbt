# -*- coding: utf-8 -*-
%w(irb/completion pp rubygems tapp what_methods awesome_print yaih).each do |l|
  begin
    require l
  rescue LoadError
  end
end

IRB.conf.update(
  :SAVE_HISTORY => 10000,
  :PROMPT_MODE => :SIMPLE
)

if defined? AwesomePrint
  IRB::Irb.class_eval do
    def output_value
      ap @context.last_value
    end
  end
end
