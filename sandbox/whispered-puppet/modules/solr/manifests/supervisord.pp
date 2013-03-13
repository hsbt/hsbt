class solr::supervisord ($install_dir, $solr_home_dir, $solr_data_dir) {
  include epel

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
  class { "solr::config":
    solr_home_dir => $solr_home_dir,
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
      Exec["unpack-solr-conf"],
      File["$solr_home_dir/solr.sh"],
      File["/etc/supervisord.conf"],
    ]
  }
}
