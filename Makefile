
colorTest: tests/color_test.ml color.ml
	ocamlfind ocamlc -o colortest -package oUnit2 -linkpkg -g  color.ml tests/color_test.ml

tupleTest: tests/tuple_test.ml tuple.ml
	ocamlfind ocamlc -o tupletest -package oUnit2 -linkpkg -g  tuple.ml tests/tuple_test.ml

canvasTest: tests/canvas_test.ml canvas.ml color.ml
	ocamlfind ocamlc -o canvastest -package oUnit2 -linkpkg -g color.ml canvas.ml tests/canvas_test.ml

test: colorTest tupleTest canvasTest
	./tupletest
	./colortest
	./canvastest

projectile: projectile.ml tuple.ml color.ml canvas.ml
	ocamlfind ocamlc -o projectile -linkpkg -g  tuple.ml color.ml canvas.ml projectile.ml
clean:
	rm -f *_test && rm *.cm* && rm -f oUnit-* && rm -f projectile