source("packages.R")
kmeans.dt <- data.table::fread("figure-kmeans-simple-data.csv")

gg <- ggplot()+
  ggtitle("K-Means, zip test data")+
  geom_point(aes(
    n.clusters, loss),
    shape=1,
    data=kmeans.dt)+
  scale_x_continuous("Model size (cluster centers)")+
  scale_y_continuous("Loss (total square loss)")
png("figure-kmeans-simple-loss.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()

selection.dt <- data.table(penaltyLearning::modelSelection(
  kmeans.dt[loss <= cummin(loss)], "loss", "n.clusters"))
gg <- ggplot()+
  geom_segment(aes(
    min.log.lambda, n.clusters,
    xend=max.log.lambda, yend=n.clusters),
    data=selection.dt)+
  scale_y_continuous("Model size (cluster centers)")+
  scale_x_continuous("Penalty")
png("figure-kmeans-simple-size.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()

