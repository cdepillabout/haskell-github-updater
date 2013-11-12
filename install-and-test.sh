#!/usr/bin/env bash

cd /var/www/github-updater/
#cabal install --bindir=`pwd`/bin && ./bin/tester
cabal install --bindir=`pwd`/bin && ( ./bin/tester & sleep .5 && ./bin/tester)
#cabal install --bindir=`pwd`/bin trash && echo && ./bin/trash
