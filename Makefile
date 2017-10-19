TARGETS := assignment-2.pdf

OS := $(shell uname)
ifeq ($(OS),Linux)
	PDFLATEX := /opt-rh7/texlive/2017/bin/x86_64-linux/pdflatex
	BIBTEX := /opt-rh7/texlive/2017/bin/x86_64-linux/bibtex
	OPEN := xdg-open
	RSCRIPT := Rscript
	DETEX := /opt-rh7/texlive/2017/bin/x86_64-linux/detex
else
	PDFLATEX := pdflatex
	BIBTEX := bibtex
	OPEN := open
	RSCRIPT := Rscript
	DETEX := detex
endif

all: $(TARGETS)

assignment-1.pdf: \
	references.bib \
	problem1b.out \
	problem1c.out \
	problem1d.out \
	problem1e.out \
	problem2b.out \
	problem3b.out \
	problem3d.out \
	problem3f.out \
	problem3h.out

assignment-2.pdf: \
	assignment-2.bib \
	problem1b.out \
	problem1b_alternative.out \
	problem1d.out \
	problem2b.out \
	problem2b_cinlars_method.out \
	problem2b_thinning.out \
	problem2d.out \
	problem3b.out \
	problem3c.out \
	problem3d.out

problem%.out: assignment-2.R
	$(RSCRIPT) -e 'pdf("$(@:.out=.pdf)"); source("$<"); $(@:.out=)()' > $@

show%: problem%.out
	@cat $<

open: all
	$(OPEN) assignment-2.pdf

check:
	$(DETEX) assignment-2.tex | diction -bs

%.eps: %.gp data.txt
	gnuplot $<

assignment-2.pdf: assignment-2.tex preamble.tex

%.pdf: %.tex
	$(PDFLATEX) -halt-on-error $<
	$(BIBTEX) $(<:.tex=)
	$(PDFLATEX) -halt-on-error $<
	$(PDFLATEX) -halt-on-error $<

clean:
	rm -f $(TARGETS) *.out $(TARGETS:.pdf=.aux) $(TARGETS:.pdf=.log) $(TARGETS:.pdf=.out)
