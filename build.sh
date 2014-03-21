#!/bin/sh

LANG=en_US.utf8 ./site rebuild
cp -R /home/madjestic/Projects/bootstrap/dist/css ./_site/
cp -R /home/madjestic/Projects/bootstrap/dist/js ./_site/
cp -R /home/madjestic/Projects/bootstrap/dist/fonts ./_site/

./site watch
