# where to install the binaries
prefix=/usr/local
exec_prefix=${prefix}
BINDIR=${exec_prefix}/bin

# where to install the man page
MANDIR=${prefix}/share/man

# other variables set by ./configure
OCAMLC   = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc
OCAMLLIB = /usr/lib/ocaml
OCAMLBEST= op

SRCS = Options.ml Util.ml Grammar.ml CombSys.ml OracleSimple.ml Tree.ml Gen.ml GParser.ml ArboLib.ml Arbogen.ml
TARGS = arbogen

all: $(TARGS)

arbogen: $(SRCS)
	$(OCAMLOPT) -o arbogen $(SRCS)


install:: arbogen
	mkdir -p $(BINDIR)
	mv arbogen $(BINDIR)
	mkdir -p $(MANDIR)/man1
	cp -f doc/$(TARGS).1 /usr/local/man/man1/$(TARGS).1

clean:
	rm arbogen
	rm *.cmi *.cmx *.o
