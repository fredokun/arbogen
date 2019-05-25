.PHONY: all build doc bench profile longtest test clean install uninstall
COMMIT = $(shell git log --pretty=format:'%h' -n 1)

all: build test

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib/arbogen lib

doc:
	dune build @doc
	[ -e doc ] || ln -sf _build/default/_doc/_html doc

test: build
	ALCOTEST_QUICK_TESTS=1 dune runtest --no-buffer

longtest: build
	dune runtest --no-buffer

bench: build
	dune exec benchs/bench.exe > bench-$(COMMIT).txt

profile:
	dune clean
	dune build --profile=profiling benchs/bench.exe
	_build/default/benchs/bench.exe
	gprof _build/default/benchs/bench.exe gmon.out > profiling-$(COMMIT).txt

install: build
	dune install

uninstall: build
	dune uninstall

clean:
	dune clean
	rm -f bin lib
