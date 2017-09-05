TARGETS = problem-1b.pdf problem-1c.pdf problem-2a.pdf

all: $(TARGETS)

%.pdf: %.R
	Rscript $< && mv Rplots.pdf $@

clean:
	rm -f $(TARGETS)
