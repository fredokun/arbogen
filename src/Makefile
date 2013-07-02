SRCS = $(wildcard *.ml *.mli *.mll *.mly)
TARGS = arbogen
KIND = native # d.byte
FLAGS = -w,Ae,-warn-error,A

all: $(TARGS)

arbogen: _build/Arbogen.$(KIND)
	cp $< $@

_build/%.$(KIND): $(SRCS)
	ocamlbuild -cflags $(FLAGS) -no-links $*.$(KIND)

clean:
	@ocamlbuild -clean
	@rm -f *~ $(TARGS)

.PHONY: all clean
