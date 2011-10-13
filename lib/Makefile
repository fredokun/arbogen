SRCS = $(wildcard *.ml *.mli)
TARGS = lib tests
KIND = native
LKIND = cmxa
FLAGS = -w,Ae,-warn-error,A

all: $(TARGS)

tests: _build/Tests.$(KIND)
	cp $< $@

lib: _build/ArboLib.$(LKIND)
        #cp $< ArboLib.$(LKIND)

_build/%.$(KIND): $(SRCS)
	ocamlbuild -cflags $(FLAGS) -no-links $*.$(KIND)

_build/%.$(LKIND): $(SRCS)
	ocamlbuild -cflags $(FLAGS) -no-links $*.$(LKIND)

clean:
	@ocamlbuild -clean
	@rm -f *~ $(TARGS)

.PHONY: all clean
