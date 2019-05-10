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
timing.stats[, mean.minutes := mean/60]
timing.stats[, mean.hours := mean.minutes/60]
print(timing.stats)

ref.dt <- data.table(
  seconds=c(1, 60),
  label=paste(" 1", c("second", "minute")))
max.dt <- timing.stats[N==1e5]
algo.colors <- c(
  linear="black",
  quadratic="blue",
  Approx_grid="red")
gg <- ggplot()+
  geom_hline(aes(
    yintercept=seconds),
    data=ref.dt,
    color="grey")+
  scale_color_manual(values=algo.colors)+
  scale_fill_manual(values=algo.colors)+
  geom_text(aes(
    1e1, seconds, label=label),
    data=ref.dt,
    color="grey",
    hjust=0,
    vjust=-0.5)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    N, mean, color=expr),
    data=timing.stats)+
  geom_text(aes(
    N, mean, color=expr, label=paste0(ifelse(
      expr=="linear",
      paste(round(mean*1e3), "ms"),
      paste(round(mean.minutes), "minutes")), "  ")),
    hjust=1,
    vjust=0,
    data=max.dt)+
  geom_point(aes(
    N, mean, color=expr),
    data=max.dt)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=timing.stats)+
  scale_y_log10("Time to compute an exact representation
 of the model selection function (seconds)")+
  scale_x_log10(
    "N = number of input models to select",
    limits=c(NA, 5e6)
    )
(dl <- directlabels::direct.label(gg, "last.polygons"))
png("figure-fullpath-db-timing.png", 4, 3.2, units="in", res=300)
print(dl)
dev.off()
