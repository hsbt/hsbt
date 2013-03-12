# Whispered

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
