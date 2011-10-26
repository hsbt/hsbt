# -*- coding: utf-8 -*-
%w(irb/completion pp rubygems tapp what_methods).each do |l|
	begin
		require l
	rescue LoadError
	end
end

IRB.conf.update(
	:SAVE_HISTORY => 10000,
	:PROMPT_MODE => :SIMPLE
)
