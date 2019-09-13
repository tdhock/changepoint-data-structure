source("packages.R")

timing.dt <- readRDS("fullpath.db.binseg.rds")

timing.dt[, seconds := time /1e9]
ggplot()+
  geom_point(aes(
    N, seconds, color=expr),
    data=timing.dt)

##lite        dark
expr.colors <- c(
  binseg.linear="grey50", linear="black",
  binseg.quadratic="#A6CEE3", quadratic="#1F78B4",#blue
  "#B2DF8A", "#33A02C",#green
  "#FB9A99", "#E31A1C",#red
  "#FDBF6F", "#FF7F00",#orange
  binseg="#CAB2D6", "#6A3D9A",#purple
  "#FFFF99", "#B15928")#yellow/brown
stats.dt <- timing.dt[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N)]
old.levs <- levels(stats.dt$expr)
step.vec <- ifelse(
  grepl(".", old.levs, fixed=TRUE),
  "step1+step2",
  ifelse(old.levs=="binseg", "step1", "step2"))
(new.levs <- structure(paste0(step.vec, "\n", old.levs), names=old.levs))
stats.dt[, algo := new.levs[expr] ]
algo.colors <- structure(expr.colors, names=new.levs[names(expr.colors)])
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
    N, mean, color=algo),
    data=stats.dt)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=algo),
    alpha=0.5,
    data=stats.dt)+
  scale_x_log10(
    "N = number of data and models (log scale)",
    breaks=stats.dt[, 10^seq(log10(min(N)), log10(max(N)))],
    limits=c(NA, max(stats.dt$N)*10))+
  scale_y_log10(
    "Computation time (seconds, log scale)",
    limits=c(NA, 1e3),
    breaks=10^seq(-2, 2)
  )+
  scale_color_manual(values=algo.colors)+
  scale_fill_manual(values=algo.colors)
dl <- directlabels::direct.label(gg, list(cex=0.6, "last.polygons"))
png("figure-fullpath-db-binseg.png", 3.6, 3, units="in", res=200)
print(dl)
dev.off()
