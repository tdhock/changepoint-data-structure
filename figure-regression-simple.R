source("packages.R")

reg.simple <- data.table::fread("figure-regression-simple-data.csv")
train.dt <- reg.simple[set != "test"]

min.dt <- train.dt[, .SD[which.min(loss)], by=set]
ggplot(mapping=aes(
  model.size, loss, color=set))+
  geom_line(data=train.dt)+
  geom_point(data=min.dt)

train.dt[, is.min := loss == min(loss), by=set]
ggplot(mapping=aes(
  model.size, loss, fill=set, color=is.min))+
  geom_point(
    data=train.dt,
    shape=21)+
  scale_color_manual(values=c("TRUE"="black", "FALSE"="grey"))

gg <- ggplot(mapping=aes(
  model.size, loss))+
  geom_point(
    data=train.dt[set=="subtrain"],
    shape=21)+
  ggtitle("Logistic regression, spam data")+
  scale_x_continuous("Model size (selected features)")+
  scale_y_continuous("Loss (logistic loss on train set)")
png('figure-regression-simple-loss.png', width=3, height=3, res=200, units="in")
print(gg)
dev.off()

selection.input <- train.dt[set=="subtrain", .(subtrain.loss=loss, model.size)]
model.dt <- data.table(penaltyLearning::modelSelection(
  selection.input,
  "subtrain.loss", "model.size"))
gg <- ggplot()+
  geom_segment(aes(
    min.log.lambda, model.size,
    xend=max.log.lambda, yend=model.size),
    data=model.dt)+
  scale_x_continuous("Log(Penalty)")+
  scale_y_continuous("Model size (selected features)")
png('figure-regression-simple-size.png', width=3, height=3, res=200, units="in")
print(gg)
dev.off()

loss.dt <- model.dt[, .(
  na.or.loss=subtrain.loss, model.size
)][selection.input, on="model.size"]
loss.dt[, selected := ifelse(is.na(na.or.loss), "no", "yes")]
gg <- ggplot(mapping=aes(
  model.size, subtrain.loss, color=selected))+
  geom_point(
    data=loss.dt,
    shape=21)+
  ggtitle("Logistic regression, spam data")+
  scale_x_continuous("Model size (selected features)")+
  scale_y_continuous("Loss (logistic loss on train set)")
png('figure-regression-simple-loss-selected.png', width=3, height=3, res=200, units="in")
print(gg)
dev.off()

tall.dt <- model.dt[train.dt, on=.(model.size)]
tall.dt[, `selected features` := model.size]
taller.dt <- data.table::melt(
  tall.dt, measure=c("loss", "selected features"))
gg <- ggplot()+
  ggtitle("Regression model selection")+
  facet_grid(variable ~ ., scales="free")+
  geom_segment(aes(
    min.log.lambda, value,
    color=set,
    xend=max.log.lambda, yend=value),
    data=taller.dt[!(variable=="model.size" & set=="validation")])+
  geom_text(aes(
    min.log.lambda, value,
    label=set,
    color=set),
    hjust=0,
    vjust=-0.5,
    data=taller.dt[variable=="loss" & min.log.lambda == -Inf])+
  theme(legend.position="none")+
  scale_x_continuous("penalty")+
  scale_y_continuous("")
png("figure-regression-simple.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()
