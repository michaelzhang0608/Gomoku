MODULES=bot game main go
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' -pkgs ANSITerminal
MAIN = main.byte
PKGS=unix,ounit2,str,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

bisect: clean test
	bisect-ppx-report html

zip:
	zip finalproject.zip *.ml* _tags Makefile .merlin .ocamlinit

clean:
	ocamlbuild -clean
	rm -f finalproject.zip _coverage bisect*.coverage

play:	
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

