
OCAMLPATH= /usr/bin

OCAMLC = $(OCAMLPATH)/ocamlc
OCFLAGS = -g #-annot

all: util combsys oracle tests rule grammar tree gen

util: Util.cmo
combsys: CombSys.cmo
oracle: Oracle.cmo
rule: Rule.cmo
grammar: Grammar.cmo
tree: Tree.cmo
gen: Gen.cmo

Util.cmo: Util.ml
	$(OCAMLC) -c $(OCFLAGS) Util.ml -o Util.cmo

CombSys.cmo: Util.ml CombSys.ml
	$(OCAMLC) -c $(OCFLAGS) CombSys.ml -o CombSys.cmo

Oracle.cmo: Util.ml CombSys.ml Oracle.ml
	$(OCAMLC) -c $(OCFLAGS) Oracle.ml -o Oracle.cmo

Rule.cmo: Util.ml CombSys.ml Oracle.ml Rule.ml
	$(OCAMLC) -c $(OCFLAGS) Rule.ml -o Rule.cmo

Tree.cmo: Util.ml CombSys.ml Oracle.ml Rule.ml Tree.ml
	$(OCAMLC) -c $(OCFLAGS) Tree.ml -o Tree.cmo

Grammar.cmo: Util.ml CombSys.ml Oracle.ml Rule.ml Tree.ml Grammar.ml
	$(OCAMLC) -c $(OCFLAGS) Grammar.ml -o Grammar.cmo

Gen.cmo: Util.ml CombSys.ml Oracle.ml Rule.ml Tree.ml Grammar.ml Gen.ml
	$(OCAMLC) -c $(OCFLAGS) Gen.ml -o Gen.cmo

tests: Util.cmo CombSys.cmo Oracle.cmo Rule.cmo Tree.cmo Grammar.cmo Gen.cmo Tests.ml
	$(OCAMLC) $(OCFLAGS) Util.cmo CombSys.cmo Oracle.cmo Rule.cmo Tree.cmo Grammar.cmo Gen.cmo Tests.ml -o Tests

clean:
	rm -f Util.cm[oi]
	rm -f CombSys.cm[oi]
	rm -f Oracle.cm[oi]
	rm -f Rule.cm[oi]
	rm -f Tree.cm[oi]
	rm -f Tests.cm[oi]
	rm -f *.annot
	rm -f *~

cleanall: clean
	rm -f Tests


