figures.pdf: figures.tex \
figure-three-iterations.tex \
figure-loss-small-evals.tex \
figure-fullpath-db-binseg.png figure-fullpath-grid-timing.png \
figure-chipseq-cv.png \
figure-binseg-quadratic-rigaill.tex \
figure-kmeans-simple-loss.png figure-pca-simple-loss.png figure-regression-simple-loss.png figure-kmeans-simple-size.png figure-pca-simple-size.png figure-regression-simple-size.png \
figure-crops-compare.png
	pdflatex figures
figure-crops-compare.png: figure-crops-compare.R figure-crops-compare-data.rds
	R --vanilla < $<
figure-crops-compare-data.rds: figure-crops-compare-data.R
	R --vanilla < $<
figure-kmeans-simple.png: figure-kmeans-simple.R figure-kmeans-simple-data.rds
	R --vanilla < $<
figure-kmeans-simple-data.rds: figure-kmeans-simple-data.R
	R --vanilla < $<
figure-pca-simple.png: figure-pca-simple.R figure-pca-simple-data.rds
	R --vanilla < $<
figure-pca-simple-data.rds: figure-pca-simple-data.R
	R --vanilla < $<
figure-regression-simple.png: figure-regression-simple.R figure-regression-simple-data.rds
	R --vanilla < $<
figure-regression-simple-data.rds: figure-regression-simple-data.R
	R --vanilla < $<
figure-binseg-quadratic-rigaill.tex: figure-binseg-quadratic-rigaill.R binseg.quadratic.rigaill.rds
	R --vanilla < $<
binseg.quadratic.rigaill.rds: binseg.quadratic.rigaill.R
	R --vanilla < $<
binseg.bug.valgrind.txt: binseg.bug.R
	R --vanilla -d valgrind < binseg.bug.R
figure-chipseq-cv.png: figure-chipseq-cv.R chipseq.cv.rds
	R --vanilla < $<
figure-three-iterations.tex: figure-three-iterations.R
	R --vanilla < $<
figure-fullpath-grid-timing.png: figure-fullpath-grid-timing.R fullpath.grid.timing.rds fullpath.db.timing.rds
	R --vanilla < $<
figure-fullpath-db-timing.png: figure-fullpath-db-timing.R fullpath.db.timing.rds
	R --vanilla < $<
figure-fullpath-db-binseg.png: figure-fullpath-db-binseg.R fullpath.db.binseg.rds
	R --vanilla < $<
chipseq.cv.rds: chipseq.cv.R chipseq.grid.rds
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
fullpath.db.binseg.rds: fullpath.db.binseg.R
	R --vanilla < $<
figure-loss-small-data/index.html: figure-loss-small-data.R loss.small.rds
	R --vanilla < $<
figure-loss-small-evals.tex: figure-loss-small-evals.R loss.small.evals.rds
	R --vanilla < $<
figure-loss-small.png: figure-loss-small.R loss.small.rds
	R --vanilla < $<
loss.small.evals.rds: loss.small.evals.R loss.small.rds
	R --vanilla < $<
loss.small.rds: loss.small.R
	R --vanilla < $<

