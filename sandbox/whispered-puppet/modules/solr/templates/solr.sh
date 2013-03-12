#!/bin/bash
cd <%= @start_dir%>
java -Xms1024m -Xmx1024m -Djetty.port=8983 -Dsolr.solr.home=<%= @solr_home_dir %> -Dsolr.data.dir=<%= @solr_data_dir %> -jar start.jar
