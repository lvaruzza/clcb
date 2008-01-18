
all: paper.pdf

paper.aux: paper.tex
	pdflatex $<

paper.bbl: $(wildcard *.bib) paper.aux
	bibtex paper 

paper.pdf: paper.tex paper.bbl
	pdflatex $<
	bibtex paper
	pdflatex $<

look: paper.pdf
	`which xpdf || which kpdf || which acroread` paper.pdf

.PHONY: look
