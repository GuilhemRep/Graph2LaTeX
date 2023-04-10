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
	ocamlc -c vestale.ml
	ocamlc -o descente Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo descente.cmo
	ocamlc -o vestale Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo vestale.cmo
	./vestale
	pdflatex test.tex

clean:
	rm -rf exemple *.mli *.cmi *.cmo *.pdf *.aux *.log *~
