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

timing.dt.list <- list()

N.vec <- c(10, 100, 1000, seq(1e4, 10e4, by=1e4), nrow(one.dt))
new.N <- N.vec[! N.vec %in% names(timing.dt.list)]
for(N in new.N){
  sub.dt <- one.dt[1:N]
  print(N)
  x <- rnorm(N)
  timing.dt.list[[paste(N)]] <- data.table(N, microbenchmark(
    binseg=fpop::multiBinSeg(x, N-1),
    linear=.C(
      "modelSelectionFwd_interface",
      loss=as.double(sub.dt$total.loss),
      complexity=as.double(sub.dt$peaks),
      N=as.integer(nrow(sub.dt)),
      models=integer(nrow(sub.dt)),
      breaks=double(nrow(sub.dt)),
      evals=integer(nrow(sub.dt)),
      PACKAGE="penaltyLearning"),
    quadratic=.C(
      "modelSelectionQuadratic_interface",
      loss=as.double(sub.dt$total.loss),
      complexity=as.double(sub.dt$peaks),
      N=as.integer(nrow(sub.dt)),
      models=integer(nrow(sub.dt)),
      breaks=double(nrow(sub.dt)),
      PACKAGE="penaltyLearning"),
    times=5))
}
timing.dt <- do.call(rbind, timing.dt.list)

library(ggplot2)
timing.dt[, seconds := time /1e9]
ggplot()+
  geom_point(aes(
    N, seconds, color=expr),
    data=timing.dt[expr %in% c("linear", "quadratic")])

stats.dt <- timing.dt[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N)]
stats.wide <- dcast(stats.dt, N ~ expr, value.var="mean")
stats.wide[, `binseg+linear` := binseg+linear]
stats.wide[, `binseg+quadratic` := binseg+quadratic]
stats.sum <- melt(
  stats.wide,
  measure.vars=c("binseg+linear", "binseg+quadratic"),
  variable.name="expr",
  value.name="mean")
gg <- ggplot()+
  geom_line(aes(
    N, mean, color=expr),
    data=rbind(
      stats.sum[, .(N, mean, expr)],
      stats.dt[, .(N, mean, expr)]))+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=stats.dt)+
  scale_x_log10(limits=c(NA, N*10))+
  scale_y_log10()
directlabels::direct.label(gg, "last.polygons")

saveRDS(timing.dt, "fullpath.db.binseg.rds")
