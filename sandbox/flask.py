# conding: utf-8

import urlparse
import json
import StringIO
from oauth2 import Client, Token, Consumer
from flask import Flask, redirect, session, request, abort

app = Flask(__name__)
app.secret_key = 'secret key'

CONSUMER_KEY = "consumer key"
CONSUMER_SECRET = "consumer secret"

REQUEST_TOKEN_URL = "http://www.tumblr.com/oauth/request_token"
ACCESS_TOKEN_URL = "http://www.tumblr.com/oauth/access_token"
AUTHORIZE_URL = "http://www.tumblr.com/oauth/authorize"

consumer = Consumer(CONSUMER_KEY, CONSUMER_SECRET)
client = Client(consumer, None)

@app.route('/')
def index():
    """
    Register
    """
    out = StringIO.StringIO()
    out.write('<html><body>')
    out.write('Tumblr OAuth Test')
    out.write('<form action="/auth" method="POST">')
    out.write('<input type="submit" value="Register"/>')
    out.write('</form>')
    out.write('</body></html>')
    s = out.getvalue()
    out.close()
    return s

@app.route('/auth', methods=['POST'])
def auth():
    """
    Request Token
    """
    result = client.request(REQUEST_TOKEN_URL, 'POST')
    request_token_map = urlparse.parse_qs(result[1])
    request_token = Token(request_token_map['oauth_token'][0],
                          request_token_map['oauth_token_secret'][0])
    session['request_token'] = request_token
    redirect_url = AUTHORIZE_URL + '?oauth_token=' + request_token.key
    return redirect(redirect_url)

@app.route('/auth_ok')
def auth_ok():
    verifier = request.args.get('oauth_verifier')
    if not 'request_token' in session:
        abort(400)
    request_token = session['request_token']
    request_token.set_verifier(verifier)
    client.token = request_token
    result = client.request(ACCESS_TOKEN_URL, 'POST')
    access_token_map = urlparse.parse_qs(result[1])
    request_token = Token(access_token_map['oauth_token'][0],
                          access_token_map['oauth_token_secret'][0])
    client.token = request_token

    info_url = 'http://api.tumblr.com/v2/user/info'
    result = client.request(info_url, 'POST')
    dict = json.loads(result[1])
    name = dict['response']['user']['name']
    blogs = [blog['title'] for blog in dict['response']['user']['blogs']]

    out = StringIO.StringIO()
    out.write('Welcome, %s !<br>' % name)
    out.write('Your Blogs: ')
    out.write('<ul>')
    for title in blogs:
        out.write('<li>%s</li>' % title)
    out.write('</ul>')
    s = out.getvalue()
    out.close()
    return s

app.run()
