data("neuroblastoma", package="neuroblastoma")
library(data.table)
nb.dt <- data.table(neuroblastoma$profiles)

## Display counts of consective same data, and total number of data
## points.
count.dt <- nb.dt[, list(
  same=sum(diff(logratio)==0),
  count=.N
), by=list(profile.id, chromosome)]
count.dt[order(same, -count)][count<1000]
count.dt[order(count)]

big.dt <- count.dt[count>1000]
nb.big <- nb.dt[big.dt, on=list(profile.id, chromosome)]
loss.big <- nb.big[, {
  print(.GRP)
  max.segments <- .N
  fit <- jointseg::Fpsn(logratio, max.segments)
  loss.dt <- with(fit, data.table(
    changes=(1:max.segments)-1,
    loss=J.est))
  selected <- penaltyLearning::modelSelection(loss.dt, complexity="changes")
  loss.dt[, selected := ifelse(changes %in% selected$changes, "yes", "no")]
  loss.dt
}, by=list(profile.id, chromosome)]

saveRDS(loss.big, "loss.big.rds")

