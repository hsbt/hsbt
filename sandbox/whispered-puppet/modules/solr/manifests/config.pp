class solr::config ($solr_home_dir) {
  file { "$solr_home_dir/solr-conf.tgz":
    ensure => present,
    source => "puppet:///modules/solr/solr-conf.tgz",
    require => Exec["create-solr-home"],
  }

  exec { "unpack-solr-conf":
    command => "tar xf $solr_home_dir/solr-conf.tgz -C $solr_home_dir",
    path => "/bin",
    refreshonly => true,
    subscribe => File["$solr_home_dir/solr-conf.tgz"],
    require => File["$solr_home_dir/solr-conf.tgz"],
  }
}
