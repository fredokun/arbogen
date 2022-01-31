.PHONY: build doc bench profile longtest test clean
COMMIT = $(shell git log --pretty=format:'%h' -n 1)

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib/arbogen lib

doc:
	dune build @doc
	[ -e doc ] || ln -sf _build/default/_doc/_html doc
	@echo Documentation available at doc/index.html

test: build
	dune runtest --no-buffer

bench: build
	dune exec benchs/bench.exe > bench-$(COMMIT).txt

profile:
	dune clean
	dune build benchs/bench.exe
	perf record --call-graph=dwarg -- _build/default/benchs/bench.exe
	@echo Run `perf report` to see the profiling results

clean:
	dune clean
	rm -f bin lib doc
