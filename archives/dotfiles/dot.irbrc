# -*- coding: utf-8 -*-
IRB.conf.update(
  :SAVE_HISTORY => 10000,
  :PROMPT_MODE => :SIMPLE
)
IRB.conf[:LOAD_MODULES] = 'irb/completion', 'pp', 'rubygems', 'awesome_print'

IRB::Irb.class_eval do
  def output_value
    ap @context.last_value
  end
end
