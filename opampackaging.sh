#!/bin/sh
#opampackaging.sh
#creates an opam package
#the verision is passed as a parameter

file=arbogen.$1
mkdir $file

cat <<EOF > $file/descr
Random tree generator to be continued
EOF
cat <<EOF > $file/opam
opam-version: "1"
maintainer: "Frédéric Peschanski <Frederic.Peschanski@lip6.fr>"
build: [
  ["make -C src/ all"]
  ["make -C src/ install"]
]
EOF
cat <<EOF > $file/url
to be decided later can be git repo but master needs to be updated first
EOF