PDFVIEWERS = evince acroread kpdf xpdf open

.SUFFIXES: .dot .tex .pdf

FIGURES=$(patsubst %.dot,%.tex,$(wildcard *.dot))

handbook.pdf: $(wildcard *.tex) $(wildcard *.bib) $(FIGURES)
	echo "The figures are $(FIGURES)"
	exit
	pdflatex handbook.tex
	bibtex handbook
	pdflatex handbook.tex

%.tex: %.dot
	dot2tex --figonly $< -o $@

look: handbook.pdf
	app=$(shell $(foreach l,${PDFVIEWERS},which $l ||) which false); \
	$$app handbook.pdf || echo -e "Could not find PDF viewer."\
	                              "Please install or edit Makefile."
