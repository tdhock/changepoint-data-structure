library(data.table)
library(penaltyLearning)
N <- 5
t <- 1:N

## L_t = N-t + I[1 < t < N]/2
dt <- data.table(loss=N-t+(1 < t & t < N)/2, complexity=t)
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
print(result)# 0 1 2 2 2
sum(result$evals)
qresult <- .C(
  "modelSelectionQuadratic_interface",
  loss=as.double(dt$loss),
  complexity=as.double(dt$complexity),
  N=as.integer(nrow(dt)),
  models=integer(nrow(dt)),
  breaks=double(nrow(dt)),
  PACKAGE="penaltyLearning")
print(qresult)

dt <- data.table(loss=N-t+N*(t!=N), complexity=t)
modelSelection(dt)
result <- .C(
  "modelSelectionFwd_interface",
  loss=as.double(dt$loss),
  complexity=as.double(dt$complexity),
  N=as.integer(nrow(dt)),
  models=integer(nrow(dt)),
  breaks=double(nrow(dt)),
  evals=integer(nrow(dt)),
  PACKAGE="penaltyLearning")
print(result) # 0 1 1 1 4
sum(result$evals)
qresult <- .C(
  "modelSelectionQuadratic_interface",
  loss=as.double(dt$loss),
  complexity=as.double(dt$complexity),
  N=as.integer(nrow(dt)),
  models=integer(nrow(dt)),
  breaks=double(nrow(dt)),
  PACKAGE="penaltyLearning")
print(qresult)
