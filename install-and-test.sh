#!/usr/bin/env bash

cd /var/www/github-updater/
cabal install --bindir=`pwd`/bin && ( ./bin/tester & sleep .5 && ./bin/tester)
