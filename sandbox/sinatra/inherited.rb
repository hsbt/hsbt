# -*- coding: utf-8 -*-
require 'sinatra/base'

class ApplicationController < Sinatra::Base
  def self.inherited(subclass)
    super
    p subclass
    use subclass
  end

  enable :logging
end

class ExampleController < ApplicationController
  get('/example') { "Example!" }
end

# 動的に生成されるアプリケーションとも連携する
Sinatra.new ApplicationController do
  get '/' do
    "See the <a href='/example'>example</a>."
  end
end

ApplicationController.run!
