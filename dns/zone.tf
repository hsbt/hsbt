resource "aws_route53_zone" "hsbt-org-public" {
    name       = "hsbt.org"
    comment    = ""

    tags {
    }
}

resource "aws_route53_zone" "railsgirls-jp-public" {
    name       = "railsgirls.jp"
    comment    = ""

    tags {
    }
}

resource "aws_route53_zone" "railsgirls-tokyo-public" {
    name       = "railsgirls.tokyo"
    comment    = ""

    tags {
    }
}

resource "aws_route53_zone" "railsgirls-nagoya-public" {
    name       = "railsgirls.nagoya"
    comment    = ""

    tags {
    }
}

