class nginx ($user="whispered"){
  package { "nginx":
    ensure => present,
  }

  file { "/etc/nginx/nginx.conf":
    ensure => present,
    content => template("nginx/nginx.conf"),
    require => Package['nginx'],
  }

  file { "/home/$user/.htaccess":
    ensure => present,
    source => "puppet:///modules/nginx/dot.htaccess",
    require => User[$user],
  }

  file { "/etc/nginx/conf.d/whispered.conf":
    ensure => present,
    content => template("nginx/whispered.conf"),
    require => Package['nginx'],
  }

  service { "nginx":
    ensure => "running",
    require => [
      File["/etc/nginx/nginx.conf"],
      File["/etc/nginx/conf.d/whispered.conf"],
    ]
  }
}
