source("packages.R")

if(!file.exists("db-loss.tsv")){
  download.file(
    "https://rcdata.nau.edu/genomic-ml/fullpath/db-loss.tsv",
    "db-loss.tsv")
}
db.loss <- fread("db-loss.tsv")

count.dt <- db.loss[, list(
  models=.N
  ), by=list(prob.id)]
biggest <- count.dt[which.max(models)]
one.dt <- db.loss[biggest, on=list(prob.id)][order(peaks)][c(TRUE, 0 < diff(peaks))][total.loss==cummin(total.loss)][c(TRUE, diff(total.loss) < 0)]

exact.dt <- data.table(microbenchmark::microbenchmark(
  "Exact_linear"=.C(
    "modelSelectionFwd_interface",
    loss=as.double(one.dt$total.loss),
    complexity=as.double(one.dt$segments),
    N=as.integer(nrow(one.dt)),
    models=integer(nrow(one.dt)),
    breaks=double(nrow(one.dt)),
    evals=integer(nrow(one.dt)),
    PACKAGE="penaltyLearning"),
  "Exact_quadratic"=.C(
    "modelSelectionQuadratic_interface",
    loss=as.double(one.dt$total.loss),
    complexity=as.double(one.dt$segments),
    N=as.integer(nrow(one.dt)),
    models=integer(nrow(one.dt)),
    breaks=double(nrow(one.dt)),
    PACKAGE="penaltyLearning"),
  times=10
))

## 1 to 1463689 segments, 0 to 731844 peaks. 287443 decreasing loss values.
timing.dt <- readRDS("fullpath.grid.timing.rds")

## pen range -18  15
## loss range "-2.287049e+08" "-1.176847e+08"

timing.dt[, algorithm := "Grid search"]

ggplot()+
  geom_line(aes(
    n.grid, mean.seconds),
    data=timing.dt)+
  geom_ribbon(aes(
    n.grid, ymin=mean.seconds-sd.seconds, ymax=mean.seconds+sd.seconds),
    alpha=0.5,
    data=timing.dt)+
  scale_x_log10()+
  scale_y_log10()
