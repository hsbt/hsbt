# coding: utf-8

import oauth2 as oauth
import urlparse

ACCESS_TOKEN_URL = 'http://www.tumblr.com/oauth/access_token'
 
CONSUMER_KEY = ''
CONSUMER_SECRET = ''
REQUEST_TOKEN = ''
REQUEST_TOKEN_SECRET = ''
OAUTH_VERIFIER = ''

def main():
    consumer = oauth.Consumer(key=CONSUMER_KEY, secret=CONSUMER_SECRET)
    token = oauth.Token(REQUEST_TOKEN, REQUEST_TOKEN_SECRET)
    token.set_verifier(OAUTH_VERIFIER)
    client = oauth.Client(consumer, token)
    resp, content = client.request(ACCESS_TOKEN_URL, 'POST')
    access_token = dict(urlparse.parse_qsl(content))
    print 'Access Token: %s' % access_token['oauth_token']
    print 'Access Token Secret: %s' % access_token['oauth_token_secret']
 
if __name__ == '__main__':
    main()
