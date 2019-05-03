library(penaltyLearning)
library(data.table)
library(microbenchmark)
loss.big <- readRDS("loss.big.rds")

loss.dec <- loss.big[, {
  is.dec <- c(TRUE, diff(loss) < 0)
  data.table(loss, changes)[is.dec]
}, by=list(profile.id, chromosome)]
nrow(loss.dec)
sum(loss.big$selected=="yes")#not the same because selection removes more.

(count.dt <- loss.dec[, list(
  models=.N
), by=list(profile.id, chromosome)][order(models)])

biggest <- count.dt[which.max(models)]
one.dt <- loss.dec[biggest, on=list(profile.id, chromosome)]

(df <- microbenchmark(
  linear=.C(
    "modelSelectionFwd_interface",
    loss=as.double(one.dt$loss),
    complexity=as.double(one.dt$changes),
    N=as.integer(nrow(one.dt)),
    models=integer(nrow(one.dt)),
    breaks=double(nrow(one.dt)),
    evals=integer(nrow(one.dt)),
    PACKAGE="penaltyLearning"),
  quadratic=.C(
    "modelSelectionQuadratic_interface",
    loss=as.double(one.dt$loss),
    complexity=as.double(one.dt$changes),
    N=as.integer(nrow(one.dt)),
    models=integer(nrow(one.dt)),
    breaks=double(nrow(one.dt)),
    PACKAGE="penaltyLearning"),
  times=5))
