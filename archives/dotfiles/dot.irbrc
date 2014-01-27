# -*- coding: utf-8 -*-
IRB.conf.update(
  :SAVE_HISTORY => 10000,
  :PROMPT_MODE => :SIMPLE
)

if RUBY_VERSION > "1.9"
  IRB.conf[:LOAD_MODULES] = 'irb/completion', 'pp', 'awesome_print'

  if defined?(AwesomePrint)
    IRB::Irb.class_eval do
      def output_value
        ap @context.last_value
      end
    end
  end
end
