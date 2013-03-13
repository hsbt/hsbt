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
