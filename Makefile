CMO= lexer.cmo parser.cmo printer.cmo main.cmo x86_64.cmo compil.cmo 
GENERATED = lexer.ml printer.ml parser.ml parser.mli x86_64.ml compil.ml
FLAGS=-annot -g



prustc: $(CMO)
	ocamlc $(FLAGS) -o $@ nums.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

.mly.mli:
	menhir  -v $<

clean:
	rm -f *.cm[io] *.o *.annot *~ prustc $(GENERATED)
	rm -f parser.output parser.automaton
	
parser.ml: ast.cmi

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend 
