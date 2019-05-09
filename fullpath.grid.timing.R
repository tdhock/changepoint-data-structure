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

select.dt <- data.table(
  penaltyLearning::modelSelection(one.dt, "total.loss", "segments"))
best.segs <- function(pen.num){
  one.dt[, segments[which.min(pen.num*segments+total.loss)] ]
}
pen.range.vec <- select.dt[, c(
  floor(max.log.lambda[1]),
  ceiling(min.log.lambda[.N])
)]

timing.dt.list <- list()
n.grid <- 10
n.unique <- 0
while(n.unique < nrow(select.dt)){
  pen.grid.vec <- exp(seq(pen.range.vec[1], pen.range.vec[2], l=n.grid))
  time.df <- microbenchmark::microbenchmark(
    selected.segs.vec <- sapply(pen.grid.vec, best.segs),
    times=5)
  u.segs.vec <- unique(selected.segs.vec)
  n.unique <- length(u.segs.vec)
  seconds <- time.df$time/1e9
  dt <- data.table(
    n.grid, n.unique,
    mean.seconds=mean(seconds),
    sd.seconds=sd(seconds))
  print(dt)
  timing.dt.list[[paste(n.grid)]] <- dt
  n.grid <- n.grid*10
}
timing.dt <- do.call(rbind, timing.dt.list)

saveRDS(timing.dt, "fullpath.grid.timing.rds")
