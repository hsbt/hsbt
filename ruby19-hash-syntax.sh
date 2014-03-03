#!/usr/bin/env bash

perl -pi -e 's/\{:([\w\d_]+)(\s*)=>/{\1:/g' **/*.rb
perl -pi -e 's/\(:([\w\d_]+)(\s*)=>/(\1:/g' **/*.rb
perl -pi -e 's/\ :([\w\d_]+)(\s*)=>/ \1:/g' **/*.rb
