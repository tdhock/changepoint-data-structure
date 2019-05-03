library(data.table)
library(penaltyLearning)
N <- 10
dt <- data.table(loss.seq=(N-1):0, complexity=0:(N-1))
dt[, loss := ifelse(complexity %in% c(0, N-1), loss.seq, loss.seq+0.5)]
modelSelection(dt)

## requires while-iteration branch.
result <- .C(
  "modelSelectionFwd_interface",
  loss=as.double(dt$loss),
  complexity=as.double(dt$complexity),
  N=as.integer(nrow(dt)),
  models=integer(nrow(dt)),
  breaks=double(nrow(dt)),
  evals=integer(nrow(dt)),
  PACKAGE="penaltyLearning")
print(result)
sum(result$evals)
