data("neuroblastoma", package="neuroblastoma")
library(data.table)
library(microbenchmark)
library(fpop)
nb.dt <- data.table(neuroblastoma$profiles)

## Display counts of consective same data, and total number of data
## points.
count.dt <- nb.dt[, list(
  same=sum(diff(logratio)==0),
  count=.N
), by=list(profile.id, chromosome)]
count.dt[order(same, -count)][count<1000]
count.dt[order(count)]

## TODO run multiBinSeg + model selection linear/quadratic to show
## importance of linear time algo.
big.dt <- count.dt[count>1000]
for(pro.i in 1:nrow(big.dt)){
  info <- big.dt[pro.i]
  nb.big <- nb.dt[info, on=list(profile.id, chromosome)]
  max.segments <- nrow(nb.big)
  print(info)
  fpop::multiBinSeg(nb.big$logratio, max.segments)
  ## microbenchmark(
  ##   binseg=fpop::multiBinSeg(logratio, max.segments),
  ##   times=2)
}

saveRDS(loss.big.binseg, "loss.big.binseg.rds")

