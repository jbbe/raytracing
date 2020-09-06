
colorTest: tests/color_test.ml color.ml
	ocamlfind ocamlc -o colortest -package oUnit2 -linkpkg -g  color.ml tests/color_test.ml

tupleTest: tests/tuple_test.ml tuple.ml
	ocamlfind ocamlc -o tupletest -package oUnit2 -linkpkg -g  tuple.ml tests/tuple_test.ml

test: colorTest tupleTest
	./tupletest
	./colortest

projectile: projectile.ml tuple.ml
	ocamlfind ocamlc -o projectile -linkpkg -g  tuple.ml projectile.ml
clean:
	rm -f *_test && rm *.cm* && rm -f oUnit-* && rm -f projectile