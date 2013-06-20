class monit {
  package { "monit":
    ensure => present,
  }

  file { "/etc/nginx/conf.d/whispered.conf":
    ensure => present,
    content => template("monit/whispered.conf"),
    require => Package['monit'],
  }

  service { "monit":
    ensure => "running",
    require => [
      File["/etc/nginx/conf.d/whispered.conf"]
    ]
  }
}
