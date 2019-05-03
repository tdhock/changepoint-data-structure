library(data.table)
library(penaltyLearning)
library(microbenchmark)
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
one.dt <- db.loss[biggest, on=list(prob.id)][order(-total.loss)][total.loss==cummin(total.loss)][c(TRUE, diff(total.loss) < 0)][c(TRUE, 0 < diff(peaks))]

timing.dt.list <- list()
for(N in c(10, 100, 1000, seq(10000, 50000, by=10000))){
  sub.dt <- one.dt[1:N]
  print(N)
  timing.dt.list[[paste(N)]] <- data.table(N, microbenchmark(
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

