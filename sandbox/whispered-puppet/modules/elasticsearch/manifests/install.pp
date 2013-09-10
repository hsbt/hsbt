class elasticsearch::install {

  package { 'java-1.7.0-openjdk':
    ensure => latest,
  }

  package { 'elasticsearch':
    ensure => latest,
    source => 'https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-0.90.3.noarch.rpm',
    require => Package['java-1.7.0-openjdk'],
    provider => rpm,
  }
}
