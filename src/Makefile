OCAMLC=ocamlc
MENHIR=menhir -v
CMO=parser.cmo lexer.cmo opt.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=biocomp

all: $(BIN)

$(BIN): $(CMO)
	$(OCAMLC) -o $(BIN) $(CMO)

%.ml: %.mly
	$(MENHIR) $<

%.mli: %.mly
	$(MENHIR) $<

%.ml: %.mll
	ocamllex $<

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

.depend depend: $(GENERATED)
	ocamldep *.ml *.mli > .depend

clean:
	rm -f $(GENERATED) *.cm[io] *.automaton *.conflicts .depend

include .depend
