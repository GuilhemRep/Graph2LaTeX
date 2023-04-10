all:
	ocamlc -c Maths.ml
	ocamlc -c -i Maths.ml > Maths.mli
	ocamlc -c Graph.ml
	ocamlc -c -i Graph.ml > Graph.mli
	ocamlc -c Physics.ml
	ocamlc -c -i Physics.ml > Physics.mli
	ocamlc -c Graph2LaTeX.ml
	ocamlc -c -i Graph2LaTeX.ml > Graph2LaTeX.mli

exemple.cmo:
	ocamlc -c exemple.ml

exemple: exemple.cmo Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo exemple.ml
	ocamlc -o exemple Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo exemple.cmo
	rm -rf exemple.cmo
	./exemple
	pdflatex test.tex

descente.cmo:
	ocamlc -c descente.ml

descente: descente.cmo Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo
	ocamlc -o descente Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo descente.cmo
	rm -rf descente.cmo
	./descente
	pdflatex test.tex

vestale.cmo:
	ocamlc -c vestale.ml

vestale: vestale.cmo Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo
	ocamlc -o vestale Maths.cmo Graph.cmo Physics.cmo Graph2LaTeX.cmo vestale.cmo
	rm -rf vestale.cmo
	./vestale
	pdflatex test.tex

clean:
	rm -rf exemple *.mli *.cmi *.cmo *.aux *.log *~
