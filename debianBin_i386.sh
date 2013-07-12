#!/bin/sh
#debianBin.sh
#creates a binary debian package from source files

#testing if in correct folder
if [ ! -f configure.in ]; then
    echo "checking if in correct folder... no"
    exit 1;
else
    echo "checking if in correct folder... ok"
fi

if which dpkg >/dev/null; then 
	echo "checking for dpkg... ok"
else
	echo "checking for dpkg... no"
	exit 1;
fi

if which fakeroot >/dev/null; then
	echo "checking for fakeroot... ok"
else
	echo "checking for fakeroot... no"
	exit 1;
fi
#creating of directories
file=arbogen_`cat VERSION`_1

mkdir $file
mkdir -p $file/DEBIAN
mkdir -p $file/usr/bin
mkdir -p $file/usr/share/doc/arbogen
mkdir -p $file/usr/share/man/man1



#creation of text files
cat <<EOF > $file/DEBIAN/control
Package: arbogen
Version: `cat VERSION`
Section: ocaml
Priority: optional
Source: arbogen
Architecture: i386
Maintainer: Frédéric Peschanski <Frederic.Peschanski@lip6.fr>
Description: Generates trees 
 Trees are generated randomly in different formats depending on grammar.
EOF

#placing files in correct place
cp doc/arbogen.1 $file/usr/share/man/man1/arbogen.1
gzip --best $file/usr/share/man/man1/arbogen.1
cp LICENSE.txt $file/usr/share/doc/arbogen/copyright

#compiling 
./compile.sh
cp bin/arbogen $file/usr/bin/

chmod -R 755 $file
chmod a-x   $file/usr/share/man/man1/arbogen.1.gz
chmod a-x $file/usr/share/doc/arbogen/copyright
#packaging
fakeroot dpkg-deb --build $file

#clean up
./cleanup.sh