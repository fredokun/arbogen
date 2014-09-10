EXEC=bin/arbogen

all: $(EXEC)

$(EXEC):
	ocamlbuild src/Arbogen.native
	ocamlbuild src/Arbogen.byte
	@mkdir -p bin
	@mv Arbogen.native bin/arbogen.native
	@mv Arbogen.byte bin/arbogen.byte

.PHONY: clean

clean:
	rm -rf _build bin

