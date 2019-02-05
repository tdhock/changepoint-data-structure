figure-loss-small-data/index.html: figure-loss-small-data.R loss.small.rds
	R --vanilla < $<
figure-loss-small.png: figure-loss-small.R loss.small.rds
	R --vanilla < $<
loss.small.rds: loss.small.R
	R --vanilla < $<

