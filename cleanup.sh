#!/bin/sh
#cleanup.sh

if [ -f Makefile ]; then
    make cleanall
fi
rm -rf Makefile
rm -rf configure
rm -rf config.status
rm -rf autom4te.cache
rm -rf config.log
rm -rf arbogen_`cat VERSION`_1
rm -rf *~
