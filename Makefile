#
# Mettre la liste des fichiers .ml, .mly, .mll et .mli
# constituant le projet. Si un fichier b.ml dépend d'un fichier
# a.ml, a.ml doit se trouver avant dans la liste.
#

SOURCES =  error.ml ast.mli parser.mly lexer.mll main.ml

# Nom du binaire

EXEC = deca

#######################################################################
# Partie générique, ne pas modifier.
#######################################################################




# Compilateurs

CAMLC = ocamlc -g
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# Sources
# Un peu de magie noire pour détecter les .mly/.mll et convaincre make
# d'appliquer les bonnes règles pour générer les .ml, .mli etc. correpondants.

PARSER_MLY=$(filter %.mly, $(SOURCES))
PARSER_MLI=$(PARSER_MLY:.mly=.mli)
PARSER_ML=$(PARSER_MLY:.mly=.ml)

LEXER_MLL=$(filter %.mll, $(SOURCES))
LEXER_ML=$(LEXER_MLL:.mll=.ml)

ALL_SOURCES=$(patsubst $(LEXER_MLL), $(LEXER_ML), $(patsubst $(PARSER_MLY), $(PARSER_MLI) $(PARSER_ML), $(SOURCES)))

OBJS = $(patsubst %.ml, %.cmo, $(filter %.ml, $(ALL_SOURCES)))
OPTOBJS = $(OBJS:.cmo=.cmx)
JS_OBJS = $(subst main.cmo, , $(OBJS))

all: depend $(EXEC)

opt: depend $(EXEC).opt


.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .java .js

$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(OPTOBJS)

main_js: $(JS_OBJS) main_js.ml
	 ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax \
          -syntax camlp4o -linkpkg -o main_js $(JS_OBJS) main_js.ml 

main_js.js: main_js
	 js_of_ocaml main_js

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

$(LEXER_ML): $(LEXER_MLL)
	$(CAMLLEX) $<

$(PARSER_MLI): $(PARSER_ML)

$(PARSER_ML): $(PARSER_MLY)
	$(CAMLYACC) $<

.java.js: $(EXEC)
	./$(EXEC) $<


clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f $(PARSER_ML) $(PARSER_MLI) $(LEXER_ML)
	rm -f $(EXEC) main_js main_js.js
	rm -f $(EXEC).opt
	rm -f $(TESTS_JS)

depend: $(ALL_SOURCES)
	$(CAMLDEP) $(ALL_SOURCES) > depend

-include depend
