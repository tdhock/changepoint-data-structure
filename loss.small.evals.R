library(data.table)

loss.small <- readRDS("loss.small.rds")

nb.evals <- loss.small[, {
  is.dec <- c(TRUE, diff(loss) < 0)
  dt <- data.table(loss, changes)[is.dec]
  result <- .C(
    "modelSelectionFwd_interface",
    loss=as.double(dt$loss),
    complexity=as.double(dt$changes),
    N=as.integer(nrow(dt)),
    models=integer(nrow(dt)),
    breaks=double(nrow(dt)),
    evals=integer(nrow(dt)),
    PACKAGE="penaltyLearning")
  with(result, list(
    models.in=nrow(dt),
    models.out=N+1,
    max.evals=max(evals),
    total.evals=sum(evals)
  ))
}, by=list(profile.id, chromosome)]

saveRDS(nb.evals, "loss.small.evals.rds")
