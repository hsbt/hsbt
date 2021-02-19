require 'net/https'

https = Net::HTTP.new('userinfo.yahooapis.jp', 443)
https.use_ssl = true
#https.ca_file = '/usr/local/etc/openssl/cert.pem'
#/etc/pki/tls/cert.pem'
https.verify_mode = OpenSSL::SSL::VERIFY_PEER
https.start { response = https.get('/'); puts response.body }
