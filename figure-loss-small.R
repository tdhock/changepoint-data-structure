library(data.table)
library(animint2)

loss.small <- readRDS("loss.small.rds")

prop.dt <- loss.small[, {
  n.selected <- sum(selected=="yes")
  data.table(
    n.selected,
    max.loss=max(loss),
    prop.selected=n.selected/.N,
    models=.N)
}, by=list(profile.id, chromosome)]

set.seed(100)
(some.props <- prop.dt[
  sample(1:.N, 100)][, .SD[1], by=list(models, prop.selected)])
ggplot()+
  scale_x_log10(
    "Number of data to segment",
    breaks=c(range(some.props$models), 10, 100))+
  geom_point(aes(
    models, prop.selected),
    shape=21,
    data=some.props)

some.loss <- loss.small[some.props, on=list(profile.id, chromosome)]
some.selection <- some.loss[selected=="yes", {
  penaltyLearning::modelSelection(.SD, complexity="changes")
}, by=list(profile.id, chromosome)]
some.loss[, mean.loss := loss/models]
some.stats <- some.loss[, list(
  min=min(mean.loss),
  max=max(mean.loss)
), by=list(profile.id, chromosome)]
range(some.stats$max)#better

some.loss[, pid.chr := paste0(profile.id, ".", chromosome)]
some.loss[, penalty := 0]
some.props[, pid.chr := paste0(profile.id, ".", chromosome)]
some.selection[, pid.chr := paste0(profile.id, ".", chromosome)]
some.selection[, cost.at.min.lambda := min.lambda*changes+loss]
viz <- animint(
  title="Changepoint model selection",
  selected=ggplot()+
    theme_bw()+
    ggtitle("Click to select a data set")+
    scale_x_log10(
      "Number of data to segment",
      breaks=c(range(some.props$models), 10, 100))+
    ylab("Proportion of models selected by linear penalty")+
    geom_point(aes(
      models, prop.selected),
      shape=21,
      size=4,
      clickSelects="pid.chr",
      alpha=0.7,
      data=data.frame(some.props)),
  loss=ggplot()+
    ggtitle("Loss values for selected data set")+
    ylab("loss")+
    xlab("changes")+
    theme_bw()+
    theme_animint(update_axes=c("x", "y"))+
    scale_size_manual(values=c(yes=3, no=4))+
    geom_text(aes(
      models, max.loss, label=paste0(
        n.selected, "/", models,
        " models selected by linear penalty")),
      showSelected="pid.chr",
      hjust=1,
      data=some.props)+
    geom_point(aes(
      changes, loss, color=selected, size=selected),
      shape=21,
      fill=NA,
      showSelected="pid.chr",
      data=data.frame(some.loss)),
  lines=ggplot()+
    ggtitle("Cost functions for selected data set")+
    theme_bw()+
    theme_animint(update_axes=c("x", "y"))+
    geom_point(aes(
      penalty, loss, color=selected),
      fill=NA,
      size=4,
      showSelected="pid.chr",
      data=data.frame(some.loss))+
    geom_point(aes(
      min.lambda, cost.at.min.lambda),
      showSelected="pid.chr",
      fill=NA,
      data=data.frame(some.selection))+
    ylab("cost(penalty) = loss + penalty*changes")+
    geom_abline(aes(
      slope=changes, intercept=loss, color=selected),
      size=1,
      showSelected="pid.chr",
      chunk_vars="pid.chr",
      data=data.frame(some.loss)))
animint2dir(viz, "figure-loss-small")
##animint2gist(viz)

gg <- ggplot()+
  ggtitle(paste0(
    nrow(prop.dt), " neuroblastoma data sets"))+
  theme_bw()+
  scale_x_log10(
    "Number of data to segment",
    breaks=c(range(prop.dt$models), 10, 100))+
  geom_point(aes(
    models, prop.selected),
    shape=21,
    data=prop.dt)
png("figure-loss-small.png")
print(gg)
dev.off()
