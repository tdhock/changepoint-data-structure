library(data.table)
L1 <- 12
L2hi <- 9
L3lo <- 2
loss.dt <- rbind(
  data.table(# 2 models
    models=2,
    loss=c(L1, L2hi, 5, 0),
    complexity=0:3),
  data.table(# 4 models
    models=4,
    loss=c(L1, 5, L3lo, 0),
    complexity=0:3),
  data.table(# three models
    models=3,
    loss=c(L1, L2hi, L3lo, 0),
    complexity=0:3))
selection.dt <- loss.dt[, penaltyLearning::modelSelection(.SD), by=models]
library(ggplot2)
ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ models)+
  geom_vline(aes(
    xintercept=max.lambda),
    data=selection.dt[models==2, .(max.lambda)])+
  geom_point(aes(
    min.lambda, loss+min.lambda*complexity),
    data=selection.dt)+
  geom_abline(aes(
    slope=complexity, intercept=loss),
    data=loss.dt)

library(data.table)
L1 <- 12
L2hi <- 9
L3lo <- 2
loss.dt <- rbind(
  data.table(# 2 models
    models=2,
    loss=c(7, 4, 0),
    complexity=0:2),
  data.table(# three models
    models=3,
    loss=c(7, 2, 0),
    complexity=0:2))
selection.dt <- loss.dt[, penaltyLearning::modelSelection(.SD), by=models]
library(ggplot2)
ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ models)+
  geom_vline(aes(
    xintercept=max.lambda),
    data=selection.dt[models==2, .(max.lambda)])+
  geom_point(aes(
    min.lambda, loss+min.lambda*complexity),
    data=selection.dt)+
  geom_abline(aes(
    slope=complexity, intercept=loss),
    data=loss.dt)
