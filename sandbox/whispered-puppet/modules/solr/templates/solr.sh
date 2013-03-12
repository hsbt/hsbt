#!/bin/bash
cd <%= @start_dir%>
java -Dsolr.solr.home=<%= @solr_home_dir %> -Dsolr.data.dir=<%= @solr_data_dir %> -jar start.jar
