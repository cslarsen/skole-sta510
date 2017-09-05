TARGETS = problem-1b.pdf problem-1c.pdf problem-2a.pdf problem-2b.pdf

all: $(TARGETS)

%.pdf: %.R
	Rscript $< && mv Rplots.pdf $@

clean:
	rm -f $(TARGETS)
