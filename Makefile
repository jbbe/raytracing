SOURCES = tuple.ml color.ml canvas.ml matrices.ml transformations.ml \
			lights.ml sphere.ml rays.ml world.ml intersections.ml

TESTARGETS = colortest tupletest canvastest matricestest transtest raystest spheretest
OCAMLC = ocamlfind ocamlopt

TEST_DIR = tests

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

transtest: tests/transformations_test.ml matrices.ml tuple.ml transformations.ml
	$(OCAMLC) -o transtest $(TEST_PACKAGES)  -linkpkg -g $(SOURCES) tests/transformations_test.ml
	./transtest

raystest: tests/rays_test.ml $(SOURCES)
	$(OCAMLC) -o raystest $(TEST_PACKAGES)  -linkpkg -g $(SOURCES) tests/rays_test.ml
	./raystest

spheretest: tests/sphere_test.ml $(SOURCES)
	$(OCAMLC) -o spheretest $(TEST_PACKAGES) -linkpkg -g $(SOURCES) tests/sphere_test.ml
	./spheretest

lighttest: tests/lights_test.ml $(SOURCES)
	$(OCAMLC) -o lighttest $(TEST_PACKAGES) -linkpkg -g $(SOURCES) tests/lights_test.ml
	./lighttest

worldtest: tests/world_test.ml $(SOURCES)
	$(OCAMLC) -o worldtest $(TEST_PACKAGES) -linkpkg -g $(SOURCES) tests/world_test.ml
	./worldtest

xstest: tests/world_test.ml $(SOURCES)
	$(OCAMLC) -o xstest $(TEST_PACKAGES) -linkpkg -g $(SOURCES) tests/intersection_test.ml
	./xs

test: 
	./tupletest
	./colortest
	./canvastest
	./matricestest
	./transtest
	./raystest
	./spheretest
	./lighttest

projectile: projectile.ml $(SOURCES) 
	ocamlfind ocamlc -o projectile -linkpkg -g  tuple.ml color.ml canvas.ml projectile.ml

clock: clock.ml $(SOURCES)
	$(OCAMLC) -o clock -linkpkg -g $(SOURCES) clock.ml

flashsphere: flash_sphere.ml $(SOURCES)
	$(OCAMLC) -o flashsphere -linkpkg -g $(SOURCES) flash_sphere.ml
	./flashsphere

clean:
	rm -f *_test *.cm*  oUnit-* -f projectile flashsphere