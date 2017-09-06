OS := $(shell uname)
MAIN := bachelor

ifeq ($(OS),Linux)
	PDFLATEX := /usr/texlive/2013/bin/x86_64-linux/pdflatex
	BIBTEX := /usr/texlive/2013/bin/x86_64-linux/bibtex
	OPEN := xdg-open
else
	PDFLATEX := pdflatex
	BIBTEX := bibtex
	OPEN := open
endif

TARGETS := ex1.pdf

all: $(TARGETS)

ex1.pdf: code1

.PHONY: code1

code1:
	make -C $@

open: all
	open ex1.pdf

%.eps: %.gp data.txt
	gnuplot $<

ex1.pdf: ex1.tex preamble.tex

%.pdf: %.tex
	$(PDFLATEX) $<
	$(PDFLATEX) $<

clean:
	make -C code1 clean
	rm -f $(TARGETS) $(TARGETS:.pdf=.aux) $(TARGETS:.pdf=.log) $(TARGETS:.pdf=.out)
