#!/bin/sh
#config.sh
#if output of makefile needs to be a byte pass as parameter arbogen.byte

if [ -f configure ]; then
    echo "checking for configure file... ok"
else
    echo "checking for configure file... no"
    if which autoconf >/dev/null; then
	echo "checking for autoconf... ok"
	autoconf
	echo "creating configure file... ok"
    else
	echo "checking for autoconf... no"
    exit 1;
    fi
fi

echo "executing configure script"
./configure

if [ -f Makefile ]; then
    echo "checking for Makefile... ok"
    make $1
else
    echo "checking for Makefile... no"
    echo "error... Aborting"
    exit 1;
fi


