all:
	ocamlopt -c src/Math.ml
	ocamlfind ocamlopt -I src/ -package oUnit -linkpkg -c src/MathTest.ml

test:
	ocamlfind ocamlc -I src/ -package oUnit -linkpkg -g src/Math.ml -g src/MathTest.ml -o tests/unit src/Unit.ml
	./tests/unit
