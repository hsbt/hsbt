#!/bin/bash

cd ~

rbenv install 2.0.0-dev
# rbenv install 1.9.3-dev
rbenv each gem up
