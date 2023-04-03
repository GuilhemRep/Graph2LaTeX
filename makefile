all:
	ocamlc -c Maths.ml
	ocamlc -c -i Maths.ml > Maths.mli
	ocamlc -c Graph.ml
	ocamlc -c -i Graph.ml > Graph.mli
	ocamlc -c Physics.ml
	ocamlc -c -i Physics.ml > Physics.mli
	ocamlc -c Graph2LaTeX.ml
	ocamlc -c -i Graph2LaTeX.ml > Graph2LaTeX.mli
	ocamlc -c descente.ml
	ocamlc -o descente Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo descente.cmo

clean:
	rm -rf exemple *.mli *.cmi *.cmo *~
