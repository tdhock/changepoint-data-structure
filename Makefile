figures.pdf: figures.tex figure-loss-small-evals.tex
	pdflatex figures
figure-loss-small-data/index.html: figure-loss-small-data.R loss.small.rds
	R --vanilla < $<
figure-loss-small-evals.tex: figure-loss-small-evals.R loss.small.evals.rds
	R --vanilla < $<
figure-loss-small.png: figure-loss-small.R loss.small.rds
	R --vanilla < $<
loss.small.rds: loss.small.R
	R --vanilla < $<
loss.small.evals.rds: loss.small.evals.R
	R --vanilla < $<

