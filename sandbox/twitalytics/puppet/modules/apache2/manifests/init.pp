class apache2 {
  package { "apache2":
    ensure => present,
  }

  service { "apache2":
    ensure => running,
    require => [Package["apache2"], File["/etc/apache2/mods-enabled/proxy.conf"]]
  }

  exec { "a2enmod proxy_ajp":
    command => "a2enmod proxy_ajp",
    path => $path,
    require => Package["apache2"],
    unless => "apache2ctl -M | grep proxy_ajp"
  }

  define apache2::httpd_conf($hostname="localhost", $port="8099"){
    file { $name :
      content => template("apache2/httpd.conf.erb")
    }
  }

  apache2::httpd_conf { "/etc/apache2/httpd.conf":
    require => Exec["a2enmod proxy_ajp"]
  }

  file { "/etc/apache2/mods-enabled/proxy.conf":
    content => template("apache2/proxy.conf.erb"),
    require => File["/etc/apache2/httpd.conf"]
  }
}
