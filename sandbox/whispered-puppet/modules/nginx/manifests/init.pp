class nginx ($user="whispered"){
  package { "nginx":
    ensure => present,
  }

  file { "/etc/nginx/nginx.conf":
    ensure => present,
    content => template("nginx/nginx.conf"),
    require => Package['nginx'],
  }

  file { "/etc/nginx/conf.d/whispered.conf":
    ensure => present,
    content => template("nginx/whispered.conf"),
    require => Package['nginx'],
  }

  file { "/etc/nginx/conf.d/solr.conf":
    ensure => present,
    content => template("nginx/solr.conf"),
    require => Package['nginx'],
  }

  service { "nginx":
    ensure => "running",
    require => [
      File["/etc/nginx/nginx.conf"],
      File["/etc/nginx/conf.d/whispered.conf"],
      File["/etc/nginx/conf.d/solr.conf"]
    ]
  }
}
