class rbenv ($user="whispered") {
  user { $user:
    ensure     => "present",
    managehome => true,
  }

  package { 'git':
    ensure => present;
  }

  exec { "rbenv":
    path    => "/usr/bin",
    command => "git clone git://github.com/sstephenson/rbenv.git /home/$user/.rbenv",
    unless  => "test -d /home/$user/.rbenv",
    user => $user,
    require => Package['git'],
  }

  exec { "ruby-build":
    path    => "/usr/bin",
    command => "git clone git://github.com/sstephenson/ruby-build.git /home/$user/.rbenv/plugins/ruby-build",
    unless  => "test -d /home/$user/.rbenv/plugins/ruby-build",
    user => $user,
    require => Exec["rbenv"];
  }

  file { "/home/$user/.bash_profile":
    source  => "puppet:///modules/rbenv/.bash_profile",
    ensure => present,
    user => $user,
    require => User[$user],
  }
}
