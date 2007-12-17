
all: paper.pdf

paper.bbl: $(wildcard *.bib)
	bibtex paper 

paper.pdf: paper.tex paper.bbl
	pdflatex $<
	bibtex paper
	pdflatex $<

look: paper.pdf
	`which xpdf || which acroread || which kpdf` paper.pdf

.PHONY: look
