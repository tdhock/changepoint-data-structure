source("packages.R")

fullpath.db.timing <- readRDS("fullpath.db.timing.rds")
big.times <- fullpath.db.timing[N==max(N)]
big.times[, seconds := time/1e9]
big.stats <- big.times[, list(
  mean=mean(seconds),
  sd=sd(seconds)
  ), by=list(expr)]

## 1 to 1463689 segments, 0 to 731844 peaks. 287443 decreasing loss values.
timing.dt <- readRDS("fullpath.grid.timing.rds")

## pen range -18  15
## loss range "-2.287049e+08" "-1.176847e+08"

timing.dt[, algorithm := "Approx_grid"]
big.stats[, algorithm := paste0("Exact_", expr)]
br.vec <- 10^seq(-3, 3)
ggplot()+
  geom_line(aes(
    n.grid, mean.seconds, color=algorithm),
    data=timing.dt)+
  geom_ribbon(aes(
    n.grid, fill=algorithm,
    ymin=mean.seconds-sd.seconds, ymax=mean.seconds+sd.seconds),
    alpha=0.5,
    data=timing.dt)+
  scale_x_log10()+
  scale_y_log10(
    "",
    breaks=br.vec,
    limits=range(br.vec))+
  theme_bw()+
  geom_rect(aes(
    xmin=-Inf, xmax=Inf,
    fill=algorithm,
    ymin=mean-sd, ymax=mean+sd),
    data=big.stats)+
  geom_hline(aes(
    yintercept=mean,
    color=algorithm),
    data=big.stats)

gfac <- function(x){
  factor(x, c(10^seq(1, 4), "Exact"))
}
timing.dt[, grid.fac := gfac(n.grid)]
big.stats[, grid.fac := gfac("Exact")]
big.stats[, n.grid := Inf]

both.dt <- rbind(
  timing.dt[, .(
    mean=mean.seconds, sd=sd.seconds, algorithm, grid.fac, n.grid)],
  big.stats[, .(mean, sd, algorithm, grid.fac, n.grid)])
text.dt <- both.dt[, .SD[which.max(grid.fac)], by=list(algorithm)]
## from other file.
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
  Exact_linear="black",
  Exact_quadratic="blue",
  Approx_grid="red")
models.var <- "N = number of input models to select
(exact algorithms only)"
grid.var <- paste0(
  "Grid points used in approximate computation
N=", max(timing.stats$N), " input models to select")
xfac <- function(x){
  factor(x, c(models.var, grid.var))
}
both.dt[, xvar := xfac(grid.var)]
text.dt[, xvar := xfac(grid.var)]
timing.stats[, xvar := xfac(models.var)]
max.dt[, xvar := xfac(models.var)]
gg <- ggplot()+
  facet_grid(. ~ xvar, scales="free")+
  geom_hline(aes(
    yintercept=seconds),
    data=ref.dt,
    color="grey")+
  scale_color_manual(values=algo.colors)+
  scale_fill_manual(values=algo.colors)+
  geom_text(aes(
    1e1, seconds, label=label),
    data=data.table(ref.dt, xvar=xfac(models.var)),
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
  geom_segment(aes(
    n.grid, mean-sd,
    xend=n.grid, yend=mean+sd,
    color=algorithm),
    data=both.dt)+
  geom_point(aes(
    n.grid, mean,
    color=algorithm),
    shape=1,
    data=both.dt)+
  geom_blank(aes(
    x, y),
    data=data.table(xvar=xfac(models.var), x=5e6, y=1))+
  geom_text(aes(
    n.grid, mean,
    color=algorithm,
    label=paste0(algorithm, "  ")),
    hjust=1,
    vjust=0.5,
    data=text.dt)+
  scale_x_log10("", breaks=10^seq(1, 6))+
  guides(fill="none", color="none")+
  scale_y_log10("Time to compute model
selection function (seconds)")
(dl <- directlabels::direct.label(gg, "last.polygons"))
png("figure-fullpath-both-timing.png", 6, 2.5, units="in", res=300)
print(dl)
dev.off()

gg <- ggplot()+
  xlab(grid.var)+
  scale_y_log10(
    "Seconds to compute model selection function",
    breaks=br.vec,
    limits=range(br.vec))+
  theme_bw()+
  geom_hline(aes(
    yintercept=seconds),
    data=ref.dt,
    color="grey")+
  geom_text(aes(
    gfac("10"), seconds, label=label),
    data=ref.dt,
    color="grey50",
    hjust=0,
    vjust=1.5)+
  scale_color_manual(values=c(
    Exact_linear="black",
    Exact_quadratic="blue",
    Approx_grid="red"))+
  geom_segment(aes(
    grid.fac, mean-sd,
    xend=grid.fac, yend=mean+sd,
    color=algorithm),
    data=both.dt)+
  geom_point(aes(
    grid.fac, mean,
    color=algorithm),
    shape=1,
    data=both.dt)+
  geom_text(aes(
    grid.fac, mean,
    color=algorithm,
    label=paste0(algorithm, "  ")),
    hjust=1,
    vjust=1,
    data=text.dt)+
  guides(color="none")
print(gg)

png("figure-fullpath-grid-timing.png", 4, 3.7, units="in", res=300)
print(gg)
dev.off()
