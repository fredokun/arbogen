
all: arbogen

arbogen: install-lib
	$(MAKE) clean
	ocamlbuild -pkg arbolib src/Arbogen.native
	ocamlbuild -pkg arbolib src/Arbogen.byte
	@mkdir -p bin
	@mv Arbogen.native bin/arbogen.native
	@mv Arbogen.byte bin/arbogen.byte

lib:
	$(MAKE) clean
	ocamlbuild src/Arbolib.cmxa
	ocamlbuild src/Arbolib.cma
	ocamlbuild src/Arbolib.cmi
	@mkdir -p lib
	@cp _build/src/Arbolib.cma lib
	@cp _build/src/Arbolib.o lib
	@cp _build/src/Arbolib.a lib
	@cp _build/src/Arbolib.cmx lib
	@cp _build/src/Arbolib.cmo lib
	@cp _build/src/Arbolib.cmxa lib
	@cp _build/src/Arbolib.cmi lib

install-lib: lib uninstall
	ocamlfind install arbolib META lib/*

uninstall:
	ocamlfind remove arbolib

.PHONY: all install-lib clean lib uninstall

clean:
	rm -rf _build bin lib

