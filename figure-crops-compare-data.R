source("packages.R")
data(neuroblastoma, package="neuroblastoma")
nb.dt <- data.table(neuroblastoma$profiles)
count.dt <- nb.dt[, .(count=.N), by=.(profile.id, chromosome)][order(count)]
log.range <- count.dt[, log(range(count))]
target.counts <- data.table(
  count=exp(seq(log.range[1], log.range[2], l=10)))
meta.dt <- count.dt[target.counts, .(
  profile.id, chromosome, N.data=x.count
), roll="nearest", on="count", mult="first"]

meta.i.vec <- 1:5
meta.i.vec <- 1:nrow(meta.dt)
loss.dt.list <- list()
timing.dt.list <- list()
for(meta.i in meta.i.vec){
  m <- meta.dt[meta.i]
  pro.dt <- nb.dt[m, on=.(profile.id, chromosome)]
  plot(pro.dt$logratio)
  Sys.sleep(1)
  pdpa <- jointseg::Fpsn(pro.dt$logratio, nrow(pro.dt))
  loss.df <- data.frame(loss=pdpa[["J.est"]], complexity=seq(0, nrow(pro.dt)-1))
  timing.df <- microbenchmark::microbenchmark(
    CROPS=crops <- changepoint::cpt.mean(pro.dt$logratio, penalty="CROPS", pen.value=c(0, 1e5), method="PELT"),
    PDPA=jointseg::Fpsn(pro.dt$logratio, nrow(pro.dt)),
    selection=selection.df <- penaltyLearning::modelSelection(loss.df),
    times=3)
  timing.dt.list[[meta.i]] <- data.table(
    meta.i, m, timing.df)
  algo.end.list <- list(
    CROPS=apply(crops@cpts.full, 1, function(x)c(x[!is.na(x)], nrow(pro.dt))),
    PDPA=apply(pdpa$t.est, 1, function(x)x[!is.na(x)]))
  cumsum.vec <- cumsum(pro.dt$logratio)
  for(algorithm in names(algo.end.list)){
    end.list <- algo.end.list[[algorithm]]
    for(end.vec in end.list){
      first <- c(1, end.vec[-length(end.vec)]+1)
      total.vec <- cumsum.vec[end.vec]-c(0, cumsum.vec)[first]
      N.vec <- end.vec-first+1
      seg.mean.vec <- total.vec/N.vec
      data.mean.vec <- rep(seg.mean.vec, N.vec)
      rss <- sum((pro.dt$logratio-data.mean.vec)^2)
      N.segs <- length(end.vec)
      loss.dt.list[[paste(meta.i, algorithm, N.segs)]] <- data.table(
        meta.i, m, algorithm, N.segs, rss)
    }
  }
}
(timing.dt <- do.call(rbind, timing.dt.list))
(loss.dt <- do.call(rbind, loss.dt.list))
timing.dt[, seconds := time/1e9]
out.list <- list(timing=timing.dt, loss=loss.dt)
saveRDS(out.list, "figure-crops-compare-data.rds")

