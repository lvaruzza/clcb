
handbook.pdf: $(wildcard *.tex)
	pdflatex handbook.tex


look: handbook.pdf
	app=`which acroread || which kpdf || which xpdf`; \
	if [ -z "$$app" ]; then echo "Could not find PDF viewer." ; \
	else $$app handbook.pdf ; fi
	
