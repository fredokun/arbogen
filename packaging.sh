#!/bin/sh
#packaging.sh
#creates an install package for using the files in its directory
#the verision is passed as a parameter

pack=arbogen_`cat VERSION`

#testing if in correct folder
if [ ! -f configure.in ]; then
    echo "checking if in correct folder... no"
    exit 1;
else
    echo "checking if in correct folder... ok"
fi

#testing if need package exists
if which autoconf >/dev/null; then
    echo "checking for autoconf... ok"
else
    echo "checking for autoconf... no"
    exit 1;
fi

#creating required package
echo "creating package"
autoconf
mkdir $pack
cp -r src/ $pack
cp Makefile.in $pack/Makefile.in
cp configure $pack/configure
cp VERSION $pack/VERSION
cp LICENSE.txt $pack/LICENCE.txt
cp README.md $pack/README.md
cp AUTHORS $pack/AUTHORS
cp -r doc/ $pack
tar -cvzf $pack.tar.gz $pack

#removing unneeded files
rm -rf autom4te.cache
rm configure
rm -rf $pack