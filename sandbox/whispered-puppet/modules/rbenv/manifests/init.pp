class rbenv ($user="whispered") {
  user { $user:
    ensure     => present,
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

  yumgroup { '"Development tools"': }

  exec { "ruby-build":
    path    => "/usr/bin",
    command => "git clone git://github.com/sstephenson/ruby-build.git /home/$user/.rbenv/plugins/ruby-build",
    unless  => "test -d /home/$user/.rbenv/plugins/ruby-build",
    user => $user,
    require => [
      Exec["rbenv"],
      Yumgroup['"Development tools"']
    ],
  }

  exec { "/home/$user/.bash_profile":
    path    => ["/usr/bin", "/bin"],
    command => "echo 'export PATH=\"\$HOME/.rbenv/bin:\$PATH\"' >> ~/.bash_profile; echo 'eval \"$(rbenv init -)\"' >> ~/.bash_profile",
    unless => "grep -q rbenv /home/$user/.bash_profile",
    user => $user,
    require => User[$user],
  }

  $ruby_version = "2.0.0-p0"
  exec { "/home/$user/.rbenv/version":
    path => ["/home/$user/.rbenv/bin", "/home/$user/.rbenv/plugins/ruby-build/bin", "/usr/bin", "/bin"],
    command => "ruby-build $ruby_version ~/.rbenv/versions/$ruby_version; echo '$ruby_version' > ~/.rbenv/version",
    unless => "test -f ~/.rbenv/version",
    user => $user,
    timeout => 900,
    require => [
      Exec["/home/$user/.bash_profile"],
      Exec["ruby-build"]
    ],
  }
}
