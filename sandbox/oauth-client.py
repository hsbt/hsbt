# coding: utf-8
 
REQUEST_TOKEN_URL = 'http://www.tumblr.com/oauth/request_token'
 
CONSUMER_KEY = ''
CONSUMER_SECRET = ''
 
def main():
    from oauth2 import Client, Token, Consumer
    import urlparse 

    consumer = Consumer(key=CONSUMER_KEY, secret=CONSUMER_SECRET)
    client = Client(consumer)
    resp, content = client.request(REQUEST_TOKEN_URL, 'GET')
    request_token = dict(urlparse.parse_qsl(content))
    print 'Request Token: %s' % request_token['oauth_token']
    print 'Request Token Secret: %s' % request_token['oauth_token_secret']
 
if __name__ == '__main__':
    main()
