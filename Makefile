figure-fullpath-db-timing.png: figure-fullpath-db-timing.R fullpath.db.timing.rds
	R --vanilla < $<
chipseq.cv.rds: chipseq.cv.R
	R --vanilla < $<
chipseq.grid.rds: chipseq.grid.R
	R --vanilla < $<
neuroblastoma.cv.rds: neuroblastoma.cv.R
	R --vanilla < $<
neuroblastoma.grid.rds: neuroblastoma.grid.R
	R --vanilla < $<
fullpath.grid.timing.rds: fullpath.grid.timing.R
	R --vanilla < $<
fullpath.db.timing.rds: fullpath.db.timing.R
	R --vanilla < $<
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

