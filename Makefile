.PHONY: build clean install uninstall

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib/arbogen lib

install: build
	dune install

uninstall: build
	dune uninstall

clean:
	dune clean
	rm -f bin lib
