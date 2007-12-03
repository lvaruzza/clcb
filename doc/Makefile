PDFVIEWERS = evince acroread kpdf xpdf open

handbook.pdf: $(wildcard *.tex) $(wildcard *.bib)
	pdflatex handbook.tex
	bibtex handbook
	pdflatex handbook.tex

look: handbook.pdf
	app=$(shell $(foreach l,${PDFVIEWERS},which $l ||) which false); \
	$$app handbook.pdf || echo -e "Could not find PDF viewer."\
	                              "Please install or edit Makefile."
