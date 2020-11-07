MODULES=bot game main 
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkgs ANSITerminal

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip finalproject.zip *.ml* _tags Makefile .merlin .ocamlinit

clean:
	ocamlbuild -clean
	rm -f finalproject.zip
