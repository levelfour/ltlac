RESULT = ltlac
SOURCES = syntax.ml lexer.mll parser.mli parser.mly normal.ml main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
