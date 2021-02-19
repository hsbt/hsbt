# -*- coding: utf-8 -*-
require 'oauth2'

CONSUMER_KEY = ''
CONSUMER_SECRET = ''
REQUEST_TOKEN = ''
REQUEST_TOKEN_SECRET = ''
OAUTH_VERIFIER = ''

client = OAuth2::Client.new(CONSUMER_KEY, CONSUMER_SECRET, :site => 'http://api.tumblr.com', :token_url => "/oauth2/token")

access_token = client.auth_code.get_token(OAUTH_VERIFIER, { :redirect_uri => "http://www.hsbt.org/callback", :token_method => :post })
puts access_token.token
 
