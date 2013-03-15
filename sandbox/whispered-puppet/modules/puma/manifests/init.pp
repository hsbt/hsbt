class puma {
  file { "/etc/init.d/puma":
    owner => root,
    group => root,
    mode => "755",
    source => "puppet:///modules/puma/puma"
  }

  service { "puma":
    ensure => "running",
    require => File["/etc/init.d/puma"],
  }

  file { "/usr/local/bin/run-puma":
    owner => root,
    group => root,
    mode => "755",
    source => "puppet:///modules/puma/run-puma",
  }

  exec { "puma.conf":
    command => "touch /etc/puma.conf",
    unless => "test -f /etc/puma.conf",
    path => "/bin",
    require => [Service["puma"], File["/usr/local/bin/run-puma"]]
  }
}
