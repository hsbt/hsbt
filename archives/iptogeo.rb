require "geoip"

g = GeoIP.new("GeoLiteCity.dat")
list = {}

File.open("ip.txt") do |f|
  f.each_line do |l|
    ip = l.chomp
    list[ip] = g.city(ip)
  end
end

list.each do |k, v|
  p [k, v.postal_code, v.real_region_name, v.latitude, v.longitude]
end
