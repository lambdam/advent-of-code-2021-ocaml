watch:
	dune build --watch --terminal-persistence=clear-on-rebuild

install-opam-dependencies:
	opam install .

lock-opam-dependencies:
	opam lock .
