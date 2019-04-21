.PHONY: build test clean install uninstall

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib/arbogen lib

test:
	dune runtest --no-buffer

install: build
	dune install

uninstall: build
	dune uninstall

clean:
	dune clean
	rm -f bin lib
