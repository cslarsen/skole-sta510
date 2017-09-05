TARGETS = problem-1b.pdf

all: $(TARGETS)

%.pdf: %.R
	Rscript $< && mv Rplots.pdf $@

clean:
	rm -f $(TARGETS)
