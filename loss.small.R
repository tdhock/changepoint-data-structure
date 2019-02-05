data("neuroblastoma", package="neuroblastoma")
library(data.table)
library(ggplot2)
nb.dt <- data.table(neuroblastoma$profiles)

## First profile has some consecutive data points that are the same.
one <- nb.dt[profile.id=="1"&chromosome=="1"]
sum(diff(one$logratio)==0)
max.segments <- nrow(one)
fit <- jointseg::Fpsn(one$logratio, max.segments)
loss.dt <- with(fit, data.table(
  segments=1:max.segments,
  loss=J.est))
selected <- penaltyLearning::modelSelection(loss.dt, complexity="segments")
loss.dt[, selected := ifelse(segments %in% selected$segments, "yes", "no")]
loss.dt[, table(selected)]
loss.dt[, table(selected)/nrow(loss.dt)]
ggplot()+
  scale_size_manual(values=c(yes=1, no=2))+
  geom_point(aes(
    segments, loss, color=selected, size=selected),
    shape=21,
    data=loss.dt)
one[, i := 1:.N]
one[470:.N]

ggplot()+
  geom_point(aes(
    0, loss, color=selected),
    data=loss.dt)+
  geom_abline(aes(
    slope=segments, intercept=loss, color=selected),
    shape=21,
    data=loss.dt)

## Display counts of consective same data, and total number of data
## points.
count.dt <- nb.dt[, list(
  same=sum(diff(logratio)==0),
  count=.N
), by=list(profile.id, chromosome)]
count.dt[order(same, -count)][count<1000]
count.dt[order(count)]

## Second data set, no consecutive data which are the same.
two <- nb.dt[profile.id=="583"&chromosome=="22"]
max.segments <- nrow(two)
fit <- jointseg::Fpsn(two$logratio, max.segments)
loss.dt <- with(fit, data.table(
  changes=(1:max.segments)-1,
  loss=J.est))
selected <- penaltyLearning::modelSelection(loss.dt, complexity="changes")
loss.dt[, selected := ifelse(changes %in% selected$changes, "yes", "no")]
ggplot()+
  scale_size_manual(values=c(yes=1, no=2))+
  geom_point(aes(
    changes, loss, color=selected, size=selected),
    shape=21,
    data=loss.dt)
loss.dt[, table(selected)]
loss.dt[, table(selected)/nrow(loss.dt)]

## Third data set, really small.
## First profile has some consecutive data points that are the same.
three <- nb.dt[profile.id=="371"&chromosome=="Y"]
sum(diff(three$logratio)==0)
max.segments <- nrow(three)
fit <- jointseg::Fpsn(three$logratio, max.segments)
loss.dt <- with(fit, data.table(
  segments=1:max.segments,
  loss=J.est))
selected <- penaltyLearning::modelSelection(loss.dt, complexity="segments")
loss.dt[, selected := ifelse(segments %in% selected$segments, "yes", "no")]
loss.dt[, table(selected)]
loss.dt[, table(selected)/nrow(loss.dt)]
ggplot()+
  scale_size_manual(values=c(yes=1, no=2))+
  geom_point(aes(
    segments, loss, color=selected, size=selected),
    shape=21,
    data=loss.dt)

small.dt <- count.dt[count<1000]
nb.small <- nb.dt[small.dt, on=list(profile.id, chromosome)]
loss.small <- nb.small[, {
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

saveRDS(loss.small, "loss.small.rds")

