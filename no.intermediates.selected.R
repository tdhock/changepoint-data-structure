library(penaltyLearning)
N <- 1000000
dt <- data.table(loss.seq=(N-1):0, complexity=0:(N-1))
dt[, loss := ifelse(complexity %in% c(0, N-1), loss.seq, loss.seq+0.5)]
modelSelection(dt)
