TARGETS = \
	problem-10a.out \
	problem-10b.out \
	problem-10c.out \
	problem-10d.out \
	problem-1b.pdf \
	problem-1c.pdf \
	problem-2a.pdf \
	problem-2b.pdf \
	problem-3.out \
	problem-4c.pdf

all: $(TARGETS)

%.out: %.R
	Rscript $< > $@

%.pdf: %.R
	Rscript $< && mv Rplots.pdf $@

clean:
	rm -f $(TARGETS)
