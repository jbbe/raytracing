SOURCES = tuple.ml color.ml canvas.ml

OCAMLC = ocamlfind ocamlc

TEST_PACKAGES = -package oUnit2
colorTest: tests/color_test.ml color.ml
	$(OCAMLC) -o colortest $(TEST_PACKAGES) -linkpkg -g  color.ml tests/color_test.ml

tupleTest: tests/tuple_test.ml tuple.ml
	$(OCAMLC) -o tupletest $(TEST_PACKAGES) -linkpkg -g  tuple.ml tests/tuple_test.ml

canvasTest: tests/canvas_test.ml canvas.ml color.ml
	$(OCAMLC) -o canvastest $(TEST_PACKAGES)  -linkpkg -g color.ml canvas.ml tests/canvas_test.ml

test: colorTest tupleTest canvasTest
	./tupletest
	./colortest
	./canvastest

projectile: projectile.ml $(SOURCES) 
	ocamlfind ocamlc -o projectile -linkpkg -g  tuple.ml color.ml canvas.ml projectile.ml
clean:
	rm -f *_test *.cm*  oUnit-* -f projectile