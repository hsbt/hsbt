class solr::install ($source_url, $install_dir, $package) {
  $packages = ["curl", "tar", "java-1.7.0-openjdk"]
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
