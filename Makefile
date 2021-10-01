.PHONY: test check

build:
	dune build src

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	zip -r chess.zip . -x _build/\* .git/\*

clean:
	dune clean
	rm -f chess.zip
