source("packages.R")
pca.dt <- data.table::fread("figure-pca-simple-data.csv")

gg <- ggplot()+
  ggtitle("PCA, zip test data")+
  geom_point(aes(
    components, loss),
    shape=1,
    data=pca.dt)+
  scale_x_continuous("Model size (principal components)")+
  scale_y_continuous("Loss (total square loss)")
png("figure-pca-simple-loss.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()

selection.dt <- data.table(penaltyLearning::modelSelection(
  pca.dt, "loss", "components"))
gg <- ggplot()+
  geom_segment(aes(
    min.log.lambda, components,
    xend=max.log.lambda, yend=components),
    data=selection.dt)+
  scale_y_continuous("Model size (principal components)")+
  scale_x_continuous("Penalty")
png("figure-pca-simple-size.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()

join.dt <- selection.dt[pca.dt, on="components"]
join.dt[, Loss := i.loss]
join.dt[, selected := ifelse(is.na(loss), "no", "yes")]
ggplot()+
  theme_bw()+
  geom_point(aes(
    components, Loss, color=selected),
    shape=1,
    data=join.dt)+
  scale_color_manual(values=c(no="grey", yes="black"))

gg <- ggplot()+
  geom_segment(aes(
    min.log.lambda, components,
    xend=max.log.lambda, yend=components),
    data=data.table(selection.dt, facet="penalty"))+
  theme_bw()+
  facet_grid(. ~ facet, scales='free')+
  geom_point(aes(
    Loss, components, color=selected),
    shape=1,
    data=data.table(join.dt, facet="loss"))+
  scale_color_manual(values=c(no="grey", yes="black"))+
  scale_x_continuous("")
png("figure-pca-simple.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()
