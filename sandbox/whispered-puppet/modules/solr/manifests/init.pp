class solr::install ($source_url, $install_dir, $package) {
  $packages = ["curl", "java-1.7.0-openjdk", "tar"]
  package { $packages:
    ensure => present
  }
  $destination = "$install_dir/$package.tgz"
  exec { "download-solr":
    command => "curl -o $destination $source_url",
    unless => "test -f $destination",
    require => Package["curl"],
    path => "/usr/bin",
  }
  exec { "unpack-solr":
    command => "tar xzf $destination --directory=$install_dir",
    unless => "test -d $install_dir/$package",
    require => Exec["download-solr"],
    path => "/usr/bin:/bin",
  }
  file { "$install_dir/apache-solr":
    ensure => "link",
    target => "$install_dir/$package",
  }
}

class solr::supervisord ($install_dir, $solr_home_dir, $solr_data_dir) {
  file { "/etc/pki/rpm-gpg/RPM-GPG-KEY-EPEL-6":
    owner => root, 
    group => root, 
    mode => 0444,
    source => "puppet:///modules/yum/RPM-GPG-KEY-EPEL-6"
  }
  yumrepo { "epel":
    mirrorlist => 'http://mirrors.fedoraproject.org/mirrorlist?repo=epel-6&arch=$basearch',
    enabled => 1,
    gpgcheck => 1,
    gpgkey => "file:///etc/pki/rpm-gpg/RPM-GPG-KEY-EPEL-6",
    require => File["/etc/pki/rpm-gpg/RPM-GPG-KEY-EPEL-6"]
  }
  package { "supervisor":
    ensure => present,
    require => Yumrepo["epel"],
  }

  $start_dir = "$install_dir/apache-solr/example"
  exec { "create-solr-home":
    command => "mkdir -p $solr_home_dir",
    unless => "test -d $solr_home_dir",
    path => "/usr/bin:/bin",
  }
  file { "$solr_home_dir/solr.sh":
    ensure => present,
    owner => root, 
    group => root, 
    mode => 0755,
    content => template("solr/solr.sh"),
    require => Exec["create-solr-home"],
  }
  file { "/etc/supervisord.conf":
    ensure => present,
    content => template("solr/supervisord.conf"),
  }
  service { "supervisord":
    ensure => "running",
    require => [
      Package["supervisor"],
      File["$solr_home_dir/solr.sh"],
      File["/etc/supervisord.conf"],
    ]
  }
}

class solr (
  $source_url="http://ftp.tsukuba.wide.ad.jp/software/apache/lucene/solr/3.6.2/apache-solr-3.6.2.tgz",
  $package="apache-solr-3.6.2",
  $install_dir="/opt",
  $solr_home_dir="/opt/solr",
  $solr_data_dir="/opt/solr/data"
) {
  class { "solr::install":
    source_url => $source_url,
    install_dir => $install_dir,
    package => $package,
  }
  class { "solr::supervisord":
    install_dir => $install_dir,
    solr_home_dir => $solr_home_dir,
    solr_data_dir => $solr_data_dir,
  }
}
