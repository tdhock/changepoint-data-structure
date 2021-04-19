source("packages.R")

max.power <- 6
timing.dt.list <- list()
selected.list <- list()
N.vec <- as.integer(10^seq(2, max.power, by=0.5))
for(N in N.vec){
  j <- 1:N
  x <- sin(j)+j/N
  L <- .C(
    "binseg_normal_interface",
    data.vec=as.double(x),
    size=as.integer(length(x)),
    Kmax=as.integer(length(x)),
    end.vec=integer(length(x)),
    cost=double(length(x)),
    before.mean=double(length(x)),
    after.mean=double(length(x)),
    before.size=integer(length(x)),
    after.size=integer(length(x)),
    invalidates.index=integer(length(x)),
    invalidates.after=integer(length(x)),
    PACKAGE="binseg")
  path <- penaltyLearning::modelSelection(data.table(
    loss=L$cost,
    complexity=1:N))
  selected.list[[paste(N)]] <- data.table(
    N, models=nrow(path),
    mean.max.it=with(L, mean(ifelse(
      before.size<after.size, after.size, before.size))))
  ##timings
  binseg.loss <- function(){
    loss <- .C(
      "binseg_normal_cost_interface",
      data.vec=as.double(x),
      size=as.integer(length(x)),
      Kmax=as.integer(length(x)),
      cost=double(length(x)),
      PACKAGE="binseg")$cost
    loss[c(TRUE, diff(loss) < 0)]
  }
  loss.vec <- binseg.loss()
  complexity.vec <- seq_along(loss.vec)
  n.input <- length(loss.vec)
  micro.df <- microbenchmark(
    binseg=binseg.loss(),
    binseg.linear={
      loss.vec <- binseg.loss()
      .C(
        "modelSelectionFwd_interface",
        loss=as.double(loss.vec),
        complexity=as.double(complexity.vec),
        n.input=as.integer(n.input),
        models=integer(n.input),
        breaks=double(n.input),
        evals=integer(n.input),
        PACKAGE="penaltyLearning")
    },
    linear={
      linear <- .C(
        "modelSelectionFwd_interface",
        loss=as.double(loss.vec),
        complexity=as.double(complexity.vec),
        n.input=as.integer(n.input),
        models=integer(n.input),
        breaks=double(n.input),
        evals=integer(n.input),
        PACKAGE="penaltyLearning")
    },
    binseg.quadSometimes={
      loss.vec <- binseg.loss()
      .C(
        "modelSelectionQuadratic_interface",
        loss=as.double(loss.vec),
        complexity=as.double(complexity.vec),
        n.input=as.integer(n.input),
        models=integer(n.input),
        breaks=double(n.input),
        PACKAGE="penaltyLearning")
    },
    quadSometimes={
      quadSometimes <- .C(
        "modelSelectionQuadratic_interface",
        loss=as.double(loss.vec),
        complexity=as.double(complexity.vec),
        n.input=as.integer(n.input),
        models=integer(n.input),
        breaks=double(n.input),
        PACKAGE="penaltyLearning")
    },
    times=5)
  timing.dt.list[[paste(N)]] <- data.table(
    N, n.input, micro.df)
}
(selected <- do.call(rbind, selected.list))
(timing.dt <- do.call(rbind, timing.dt.list))

saveRDS(timing.dt, "fullpath.db.binseg.rds")
