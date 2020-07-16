#!/usr/bin/env bash

sudo rpm -Uvh http://rpms.famillecollet.com/enterprise/remi-release-5.rpm
sudo yum --enablerepo=remi --disablerepo=epel install libmemcached libmemcached-devel -y
