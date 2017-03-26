# 
# JSTEM: Makefile for Testing Parser
#  

OCAMLC = ocamlc
OBJS = ../../parser.cmo ../../scanner.cmo ../../ast.cmo parserize.cmo
INCLUDES = -I ../../

default: parserize

parserize: $(OBJS)
	$(OCAMLC) $(INCLUDES) -o parserize $(OBJS)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c $<

.PHONY: clean
clean:
	rm -f parserize *.cmo *.cmi

