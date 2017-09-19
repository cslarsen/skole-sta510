TARGETS := assignment-1.pdf

OS := $(shell uname)
ifeq ($(OS),Linux)
	PDFLATEX := /opt-rh7/texlive/2017/bin/x86_64-linux/pdflatex
	BIBTEX := /opt-rh7/texlive/2017/bin/x86_64-linux/bibtex
	OPEN := xdg-open
	RSCRIPT := Rscript
else
	PDFLATEX := pdflatex
	BIBTEX := bibtex
	OPEN := open
	RSCRIPT := Rscript
endif

all: $(TARGETS)

assignment-1.pdf: problem1b.out problem1c.out problem1d.out problem1e.out problem2b.out problem3b.out

problem%.out: assignment-1.R
	rm -f Rplots.pdf
	$(RSCRIPT) -e 'source("$<"); $(@:.out=)()' > $@
	if [ -e Rplots.pdf ]; then mv Rplots.pdf $(@:.out=.pdf); fi

open: all
	$(OPEN) assignment-1.pdf

%.eps: %.gp data.txt
	gnuplot $<

assignment-1.pdf: assignment-1.tex preamble.tex

%.pdf: %.tex
	$(PDFLATEX) -halt-on-error $<
	$(BIBTEX) $(<:.tex=)
	$(PDFLATEX) -halt-on-error $<
	$(PDFLATEX) -halt-on-error $<

clean:
	rm -f $(TARGETS) *.out $(TARGETS:.pdf=.aux) $(TARGETS:.pdf=.log) $(TARGETS:.pdf=.out)
