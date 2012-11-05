
whoami=$(shell whoami)

all: RGalaxy-vignette.pdf clean

pdfclean:
	rm -f RGalaxy-vignette.pdf

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log cache *.R
	rm -rf figure
	rm -rf *.md

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif
RPROG=	$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript


RGalaxy-vignette.pdf:
	rm -f *.R
	$(RSCRIPT) --vanilla -e "library(knitr);knit('RGalaxy-vignette.Rmd')"
	$(RSCRIPT) --vanilla -e "library(knitr);purl('RGalaxy-vignette.Rmd')"
	pandoc --css RGalaxy.css -o RGalaxy-vignette.pdf RGalaxy-vignette.md
	cp RGalaxy-vignette.md ../inst/doc
	cp RGalaxy-vignette.R ../inst/doc/RGalaxy-vignette.R.txt

	

