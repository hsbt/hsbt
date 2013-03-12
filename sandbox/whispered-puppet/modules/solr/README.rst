==================
Solr puppet module
==================

Simple puppet module for installing Solr and using Supervisord to run it.

This module:

- Downloads a specified tarball
- Unpacks the tarball
- Creates a symlink to unpacked directory
- Install supervisord 
- Configures supervisord to run solr

Install
=======

Clone the repo into your puppet modules directory.

Usage
=====

Minimal::

	class { "solr":
		solr_home_dir => "/vagrant/www/deploy/solr",
	}

Maximal::

	class { "solr":
        package => "apache-solr-3.6.1",
		source_url => "http://mirrors.enquira.co.uk/apache/lucene/solr/3.6.1/apache-solr-3.6.1.tgz",
		install_dir => "/opt",
		solr_home_dir => "/vagrant/www/deploy/solr",
		solr_data_dir => "/opt/data",
	}