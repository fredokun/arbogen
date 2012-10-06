SRCS = $(wildcard *.ml *.mli *.mll *.mly)
TARGS = arbogen tests
KIND = native # d.byte
FLAGS = -w,Ae,-warn-error,A

all: $(TARGS)

arbogen: _build/Arbogen.$(KIND)
	cp $< $@

tests: _build/Tests.$(KIND)
	cp $< $@

_build/%.$(KIND): $(SRCS)
	ocamlbuild -cflags $(FLAGS) -no-links $*.$(KIND)

clean:
	@ocamlbuild -clean
	@rm -f *~ $(TARGS)

.PHONY: all clean
