.PHONY: test check

build:
	dune build src

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	zip -r enigma.zip . -x _build/\* .git/\*

clean:
	dune clean
	rm -f enigma.zip
