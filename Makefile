EXEC=bin/arbogen

all: $(EXEC)

$(EXEC):
	ocamlbuild src/Arbogen.native
	ocamlbuild src/Arbogen.byte
	@mkdir -p bin
	@mv Arbogen.native bin/arbogen.native
	@mv Arbogen.byte bin/arbogen.byte

lib:
	ocamlbuild src/ArboLib.cma
	ocamlbuild src/ArboLib.cmxa
	ocamlbuild src/ArboLib.cmi
	@mkdir -p lib
	@mv _build/src/ArboLib.cma lib
	@mv _build/src/ArboLib.cmxa lib
	@mv _build/src/ArboLib.cmi lib

.PHONY: clean lib

clean:
	rm -rf _build bin lib

