
test: tests/tuple_test.ml tuple.ml
	ocamlfind ocamlc -o test -package oUnit2 -linkpkg -g  tuple.ml tests/tuple_test.ml

clean:
	rm -f test && rm *.cm* && rm -f oUnit-*