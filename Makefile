.PHONY: test check

build:
	dune build src

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	zip -r chess.zip . -x _build/\* .git/\*

clean:
	dune clean
	rm -f chess.zip
