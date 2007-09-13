all: FAQ-vegan.pdf decision-vegan.pdf
FAQ-vegan.pdf: FAQ-vegan.texi
	texi2dvi --pdf --clean FAQ-vegan.texi
decision-vegan.pdf: decision-vegan.tex
	texi2dvi --pdf --clean decision-vegan.tex
	-rm -f decision-vegan-0*.* Rplots.ps
