class nginx {
  package { "nginx":
    ensure => present,
  }
  file { "/etc/nginx/conf.d/default.conf":
    ensure => present,
    content => template("nginx/default.conf"),
  }
}
