class puma ($user="whispered"){
  file { "/etc/init.d/puma":
    owner => root,
    group => root,
    mode => "755",
    source => "puppet:///modules/puma/puma"
  }

  file { "/usr/local/bin/run-puma":
    owner => root,
    group => root,
    mode => "755",
    source => "puppet:///modules/puma/run-puma",
  }

  # $apppath = "/home/$user/app/whispered/current"
  # exec { "puma.conf":
  #   command => "/etc/init.d/puma add $apppath $user $apppath/config/puma.rb $apppath/log/puma.log",
  #   if => "test -f $apppath/config/puma.rb",
  #   unless => "test -f /etc/puma.conf",
  #   path => "/usr/bin",
  #   require => [
  #     File["/usr/local/bin/run-puma"],
  #     File["/etc/init.d/puma"],
  #   ]
  # }

  # service { "puma":
  #   ensure => "running",
  #   require => [
  #     Exec["puma.conf"],
  #   ]
  # }
}
