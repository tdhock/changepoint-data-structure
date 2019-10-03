source("packages.R")

timing.dt <- readRDS("binseg.quadratic.rigaill.rds")
timing.dt[, seconds := time /1e9]
timing.dt[, BinSeg := ifelse(binseg=="fast", "log-linear", "quadratic")]
stats.dt <- timing.dt[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N, BinSeg, n.selected)]
sim.fun.dt <- unique(timing.dt[, .(
  BinSeg, n.selected, latex)])
##lite        dark
expr.colors <- c(
  binseg.linear="grey50", linear="black",
  binseg.quadAlways="#A6CEE3", quadAlways="#1F78B4",#blue
  binseg.quadSometimes="#B2DF8A", quadSometimes="#33A02C",#green
  binseg.Rigaill="#FB9A99", Rigaill="#E31A1C",#red
  "#FDBF6F", "#FF7F00",#orange
  binseg="#CAB2D6", "#6A3D9A",#purple
  "#FFFF99", "#B15928")#yellow/brown
gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  scale_color_manual(values=expr.colors)+
  scale_fill_manual(values=expr.colors)+
  facet_grid(BinSeg ~ n.selected, labeller=label_both)+
  geom_text(aes(
    min(stats.dt$N), max(stats.dt$mean), label=latex),
    hjust=0,
    vjust=1,
    data=sim.fun.dt)+
  geom_line(aes(
    N, mean, color=expr),
    data=stats.dt)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=stats.dt)+
  scale_x_log10(
    "N = number of simulated data (log scale)",
    limits=c(NA, max(stats.dt$N)*10))+
  scale_y_log10(
    "Computation time (seconds, log scale)")
dl <- directlabels::direct.label(gg, "last.polygons")
print(dl)

tikz("figure-binseg-quadratic-rigaill.tex", 3.6, 3)
print(dl)
dev.off()
