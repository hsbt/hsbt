resource "aws_route53_record" "hsbt-org-A" {
    zone_id = "ZIASVYAYC132O"
    name    = "hsbt.org"
    type    = "A"
    records = ["40.74.117.115"]
    ttl     = "300"
}

resource "aws_route53_record" "hsbt-org-NS" {
    zone_id = "ZIASVYAYC132O"
    name    = "hsbt.org"
    type    = "NS"
    records = ["ns-711.awsdns-24.net.", "ns-1593.awsdns-07.co.uk.", "ns-1469.awsdns-55.org.", "ns-325.awsdns-40.com."]
    ttl     = "172800"
}

resource "aws_route53_record" "hsbt-org-SOA" {
    zone_id = "ZIASVYAYC132O"
    name    = "hsbt.org"
    type    = "SOA"
    records = ["ns-711.awsdns-24.net. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400"]
    ttl     = "900"
}

resource "aws_route53_record" "bakagaiku-hsbt-org-CNAME" {
    zone_id = "ZIASVYAYC132O"
    name    = "bakagaiku.hsbt.org"
    type    = "CNAME"
    records = ["bakagaiku.herokuapp.com"]
    ttl     = "300"
}

resource "aws_route53_record" "blog-hsbt-org-CNAME" {
    zone_id = "ZIASVYAYC132O"
    name    = "blog.hsbt.org"
    type    = "CNAME"
    records = ["domains.tumblr.com"]
    ttl     = "300"
}

resource "aws_route53_record" "www-hsbt-org-CNAME" {
    zone_id = "ZIASVYAYC132O"
    name    = "www.hsbt.org"
    type    = "CNAME"
    records = ["lagrange.japanwest.cloudapp.azure.com"]
    ttl     = "300"
}

resource "aws_route53_record" "railsgirls-jp-A" {
    zone_id = "Z3LDG81BASDFEC"
    name    = "railsgirls.jp"
    type    = "A"
    records = ["185.199.108.153", "185.199.109.153", "185.199.110.153", "185.199.111.153"]
    ttl     = "300"
}

resource "aws_route53_record" "railsgirls-jp-NS" {
    zone_id = "Z3LDG81BASDFEC"
    name    = "railsgirls.jp"
    type    = "NS"
    records = ["ns-266.awsdns-33.com.", "ns-1519.awsdns-61.org.", "ns-698.awsdns-23.net.", "ns-1574.awsdns-04.co.uk."]
    ttl     = "172800"
}

resource "aws_route53_record" "railsgirls-jp-SOA" {
    zone_id = "Z3LDG81BASDFEC"
    name    = "railsgirls.jp"
    type    = "SOA"
    records = ["ns-266.awsdns-33.com. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400"]
    ttl     = "900"
}

resource "aws_route53_record" "blog-railsgirls-jp-CNAME" {
    zone_id = "Z3LDG81BASDFEC"
    name    = "blog.railsgirls.jp"
    type    = "CNAME"
    records = ["domains.tumblr.com"]
    ttl     = "300"
}

resource "aws_route53_record" "railsgirls-tokyo-NS" {
    zone_id = "Z2P752O05M2H7V"
    name    = "railsgirls.tokyo"
    type    = "NS"
    records = ["ns-132.awsdns-16.com.", "ns-2000.awsdns-58.co.uk.", "ns-789.awsdns-34.net.", "ns-1097.awsdns-09.org."]
    ttl     = "172800"
}

resource "aws_route53_record" "railsgirls-tokyo-SOA" {
    zone_id = "Z2P752O05M2H7V"
    name    = "railsgirls.tokyo"
    type    = "SOA"
    records = ["ns-132.awsdns-16.com. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400"]
    ttl     = "900"
}

resource "aws_route53_record" "railsgirls-nagoya-NS" {
    zone_id = "Z1LUZUP94P21PC"
    name    = "railsgirls.nagoya"
    type    = "NS"
    records = ["ns-1362.awsdns-42.org.", "ns-977.awsdns-58.net.", "ns-1578.awsdns-05.co.uk.", "ns-213.awsdns-26.com."]
    ttl     = "172800"
}

resource "aws_route53_record" "railsgirls-nagoya-SOA" {
    zone_id = "Z1LUZUP94P21PC"
    name    = "railsgirls.nagoya"
    type    = "SOA"
    records = ["ns-1362.awsdns-42.org. awsdns-hostmaster.amazon.com. 1 7200 900 1209600 86400"]
    ttl     = "900"
}
