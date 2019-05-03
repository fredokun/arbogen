.PHONY: all build longtest test clean install uninstall

all: build test

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib/arbogen lib

test:
	ALCOTEST_QUICK_TESTS=1 dune runtest --no-buffer

longtest:
	dune runtest --no-buffer

install: build
	dune install

uninstall: build
	dune uninstall

clean:
	dune clean
	rm -f bin lib
