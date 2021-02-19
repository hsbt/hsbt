#!/bin/bash
cd .rbenv/versions/ree-1.8.7-2012.02/bin
gem up
yes | gem cleanup
gem i open_gem
