SOURCES = tuple.ml color.ml canvas.ml

OCAMLC = ocamlfind ocamlc

TEST_PACKAGES = -package oUnit2

colortest: tests/color_test.ml color.ml
	$(OCAMLC) -o colortest $(TEST_PACKAGES) -linkpkg -g  color.ml tests/color_test.ml

tupletest: tests/tuple_test.ml tuple.ml
	$(OCAMLC) -o tupletest $(TEST_PACKAGES) -linkpkg -g  tuple.ml tests/tuple_test.ml

canvastest: tests/canvas_test.ml color.ml canvas.ml
	$(OCAMLC) -o canvastest $(TEST_PACKAGES)  -linkpkg -g color.ml canvas.ml tests/canvas_test.ml

matricestest: tests/matrices_test.ml matrices.ml tuple.ml
	$(OCAMLC) -o matricestest $(TEST_PACKAGES)  -linkpkg -g tuple.ml matrices.ml tests/matrices_test.ml
	./matricestest

test: colortest tupletest canvastest matricestest
	./tupletest
	./colortest
	./canvastest
	./matricesTest

projectile: projectile.ml $(SOURCES) 
	ocamlfind ocamlc -o projectile -linkpkg -g  tuple.ml color.ml canvas.ml projectile.ml
clean:
	rm -f *_test *.cm*  oUnit-* -f projectile