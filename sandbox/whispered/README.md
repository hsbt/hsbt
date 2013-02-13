# Whispered

```
$ git clone git://github.com/hsbt/whispered.git
$ cd whispered
$ bundle install
```

configuring follow files:

 * config/settings/production.yml
 * config/database.yml

```
$ rake sunspot:solr:start
$ rake sunspot:reindex
$ rails s
```
