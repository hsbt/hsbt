# -*- coding: utf-8 -*-
require "oauth"

CONSUMER_KEY = ""
CONSUMER_SECRET = ""

consumer = OAuth::Consumer.new(
  CONSUMER_KEY,
  CONSUMER_SECRET,
  { site: "http://api.tumblr.com" }
)
request_token = consumer.get_request_token
access_token = request_token.get_access_token(oauth_verifier: "")
puts access_token.token
puts access_token.secret
