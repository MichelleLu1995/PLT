#
# JSTEM: Makefile for Testing Scanner
#

OCAMLC = ocamlc
OBJS = ../../scanner.cmo tokenize.cmo
INCLUDES = -I ../../

default: tokenize

tokenize: $(OBJS)
	$(OCAMLC) $(INCLUDES) -o tokenize $(OBJS)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c $<

.PHONY: clean
clean:
	rm -f tokenize *.cmo *.cmi *.cmx

