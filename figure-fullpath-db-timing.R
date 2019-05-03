library(data.table)
library(ggplot2)
fullpath.db.timing <- readRDS("fullpath.db.timing.rds")

ggplot()+
  geom_point(aes(
    N, time/1e9, color=expr),
    shape=1,
    data=fullpath.db.timing)

ggplot()+
  geom_point(aes(
    N, time/1e9, color=expr),
    shape=1,
    data=fullpath.db.timing)+
  scale_x_log10()+
  scale_y_log10()

## question: past which N is the quadratic model selection dominating
## the

fullpath.db.timing[, seconds := time/1e9]
timing.stats <- fullpath.db.timing[, list(
  mean=mean(seconds),
  sd=sd(seconds)
  ), by=list(N, expr)]

gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    N, mean, color=expr),
    data=timing.stats)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=timing.stats)+
  scale_y_log10("Seconds to compute
an exact representation
 of the model selection function")+
  scale_x_log10(
    "N = number of inputs models to select",
    limits=c(NA, 5e5)
    )
dl <- directlabels::direct.label(gg, "last.polygons")
png("figure-fullpath-db-timing.png", 4, 3, units="in", res=300)
print(dl)
dev.off()
