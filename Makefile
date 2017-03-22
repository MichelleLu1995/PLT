# 
# JSTEM: MAIN MAKEFILE
#  

OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc


scanner.ml: scanner.mll
	$(OCAMLLEX) scanner.mll

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<



test: scanner.ml
	cd tests; make


.PHONY: clean
clean:
	cd tests; make clean
	rm -f odds scanner.ml 

