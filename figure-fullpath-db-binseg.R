source("packages.R")

timing.dt <- readRDS("fullpath.db.binseg.rds")

##lite        dark
algo.colors <- c(
  binseg.linear="grey50", linear="black",
  binseg.quadratic="#A6CEE3", quadratic="#1F78B4",#blue
  "#B2DF8A", "#33A02C",#green
  "#FB9A99", "#E31A1C",#red
  "#FDBF6F", "#FF7F00",#orange
  binseg="#CAB2D6", "#6A3D9A",#purple
  "#FFFF99", "#B15928")#yellow/brown

timing.dt[, seconds := time /1e9]
ggplot()+
  geom_point(aes(
    N, seconds, color=expr),
    data=timing.dt)

stats.dt <- timing.dt[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N)]
full.names <- c(
  quadratic="quadratic\nalgo alone",
  linear="linear\nalgo alone",
  binseg="binary\nsegmentation")
ref.dt <- data.table(
  seconds=c(1, 60),
  label=paste(" 1", c("second", "minute")))
lab.dt <- stats.dt[
  J(c("quadratic", "binseg", "linear"), 10^5),
  on=.(expr, N)]
#1e5, 3.4 sec for binseg, 1.9 minutes for quadratic.
gg <- ggplot()+
  theme_bw()+
  geom_hline(aes(
    yintercept=seconds),
    data=ref.dt,
    color="grey")+
  geom_text(aes(
    100, seconds, label=label),
    data=ref.dt,
    color="grey50",
    vjust=1.5,
    hjust=0)+
  geom_line(aes(
    N, mean, color=expr),
    data=stats.dt)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=stats.dt)+
  scale_x_log10(
    "N = number of data and models (log scale)",
    breaks=stats.dt[, 10^seq(log10(min(N)), log10(max(N)))],
    limits=c(NA, max(stats.dt$N)*50))+
  scale_y_log10(
    "Computation time (seconds, log scale)"
  )+
  scale_color_manual(values=algo.colors)+
  scale_fill_manual(values=algo.colors)
dl <- directlabels::direct.label(gg, list(cex=0.8, "last.polygons"))
png("figure-fullpath-db-binseg.png", 3.6, 3, units="in", res=200)
print(dl)
dev.off()
