# Whispered

[![Build Status](https://secure.travis-ci.org/hsbt/whispered.png)](https://travis-ci.org/hsbt/whispered) [![Coverage Status](https://coveralls.io/repos/hsbt/whispered/badge.png?branch=master)](https://coveralls.io/r/hsbt/whispered) [![Dependency Status](https://gemnasium.com/hsbt/whispered.png)](https://gemnasium.com/hsbt/whispered) [![Code Climate](https://codeclimate.com/github/hsbt/whispered.png)](https://codeclimate.com/github/hsbt/whispered)

```
$ git clone git://github.com/hsbt/whispered.git
$ cd whispered
$ bundle install
```

configuring follow files:

 * config/settings/production.yml

if you use development environment, you can use sunspot_solr.

```
$ rake sunspot:solr:start
$ rake sunspot:reindex
$ rails s
```

if you need to run in production environment, you can use [whispered-puppet](https://github.com/hsbt/whispered-puppet). it is manifests of solr and supervisord.
