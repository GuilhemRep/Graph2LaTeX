all:
	ocamlc -c Graph.ml
	ocamlc -c -i Graph.ml > Graph.mli
	ocamlc -c Maths.ml
	ocamlc -c -i Maths.ml > Maths.mli
	ocamlc -c Physics.ml
	ocamlc -c -i Physics.ml > Physics.mli
	ocamlc -c Graph2LaTeX.ml
	ocamlc -c -i Graph2LaTeX.ml > Graph2LaTeX.mli
	ocamlc -c affichage.ml
	ocamlc -o test Graph.cmo Maths.cmo Physics.cmo Graph2LaTeX.cmo affichage.cmo

clean:
	rm -rf exemple *.mli *.cmi *.cmo *~
