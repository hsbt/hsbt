#!/bin/sh
# start_server (root) restarts the worker on SIGHUP, so the new worker
# reads the renewed certificate before dropping to the h2o user.
exec systemctl reload h2o
