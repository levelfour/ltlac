RESULT = ltlac
SOURCES = syntax.ml lexer.mll parser.mli parser.mly main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
