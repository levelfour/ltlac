RESULT = ltlac
SOURCES = syntax.ml lexer.mll parser.mli parser.mly \
		  ltl.ml pbfml.ml normal.ml altBuchi.ml mh84.ml \
		  dot.ml main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
