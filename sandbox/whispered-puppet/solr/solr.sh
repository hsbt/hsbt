#!/bin/bash
cd /opt/apache-solr/example
java -Dsolr.solr.home=/vagrant/solr -Dsolr.data.dir=/opt/data -jar start.jar
