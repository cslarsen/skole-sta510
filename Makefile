OS := $(shell uname)
MAIN := bachelor

ifeq ($(OS),Linux)
	PDFLATEX := /opt-rh7/texlive/2017/bin/x86_64-linux/pdflatex
	BIBTEX := /opt-rh7/texlive/2017/bin/x86_64-linux/bibtex
	OPEN := xdg-open
else
	PDFLATEX := pdflatex
	BIBTEX := bibtex
	OPEN := open
endif

TARGETS := assignment-1.pdf

all: $(TARGETS)

ex1.pdf: code1

.PHONY: code1

code1:
	make -C $@

open: all
	open assignment-1.pdf

%.eps: %.gp data.txt
	gnuplot $<

ex1.pdf: ex1.tex preamble.tex

assignment-1.pdf: assignment-1.tex preamble.tex

%.pdf: %.tex
	$(PDFLATEX) $<
	$(PDFLATEX) $<

clean:
	make -C code1 clean
	rm -f $(TARGETS) $(TARGETS:.pdf=.aux) $(TARGETS:.pdf=.log) $(TARGETS:.pdf=.out)
