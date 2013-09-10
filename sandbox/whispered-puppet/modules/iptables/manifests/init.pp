class iptables {

  package { 'iptables':
    ensure => present,
  }

  service { 'iptables':
    enable    => true,
    ensure    => running,
    hasstatus => true,
  }

  $accept_from = [
    '0.0.0.0',
  ]

  file { '/etc/sysconfig/iptables':
    ensure  => present,
    mode    => 600,
    before  => Service['iptables'],
    notify  => Service['iptables'],
    content => template('iptables'),
    require => Package['iptables'],
  }
}
