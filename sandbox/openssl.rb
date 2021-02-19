#!/usr/bin/env ruby

puts "Ruby version: #{RUBY_VERSION}"

require 'openssl'

include OpenSSL

def make_cname_cert(hostname)
  cert = X509::Certificate.new
  cert.subject = X509::Name.parse "/DC=some/DC=site/CN=#{hostname}"
  cert
end


def make_san_cert(hostname)
  ef = X509::ExtensionFactory.new
  cert = X509::Certificate.new
  cert.subject = X509::Name.parse "/DC=some/DC=site/CN=Some Site"

  # Workaround to add a null byte into a subjectAltName dNSName. OpenSSL's
  # parser/constructor for subjectAltName strings probably uses C-strings at
  # some point, making it a little more difficult to include null characters in
  # the extension.
  ext = ef.create_ext('subjectAltName', 'DNS:placeholder')
  ext_asn1 = OpenSSL::ASN1.decode(ext.to_der)
  san_list_der = ext_asn1.value.reduce(nil) { |memo,val| val.tag == 4 ? val.value : memo }
  san_list_asn1 = OpenSSL::ASN1.decode(san_list_der)
  san_list_asn1.value[0].value = hostname
  ext_asn1.value[1].value = san_list_asn1.to_der
  real_ext = OpenSSL::X509::Extension.new ext_asn1

  cert.add_extension(real_ext)

  cert
end



["www.example.com", "www.example.com\0.evil.com"].each do |hostname|
  puts ""
  puts "CNAME entry: #{hostname.inspect}"
  cert = make_cname_cert(hostname)
  subject = cert.subject
  puts "subject's DER-encoding: #{subject.to_der.inspect}"
  puts "OpenSSL's string representation of subject: #{subject.inspect.inspect}"
  cname = subject.to_a.find { |ent| ent[0] == 'CN' }[1]
  puts "OpenSSL's string representation of subject's CNAME: #{cname.inspect}"
  puts "Does the cert match 'www.example.com': #{ SSL.verify_certificate_identity(cert, 'www.example.com')}"

  puts ""
  puts "subjectAltName dNSName entry: #{hostname.inspect}"
  cert = make_san_cert(hostname)
  ext = cert.extensions.find { |ext| ext.oid == 'subjectAltName' }
  puts "subjectAltName's DER-encoding: #{ext.to_der.inspect}"
  puts "OpenSSL's string representation of subjectAltName's value: #{ext.value.inspect}"
  puts "Does the cert match 'www.example.com': #{ SSL.verify_certificate_identity(cert, 'www.example.com')}"
end
