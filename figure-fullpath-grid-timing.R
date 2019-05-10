source("packages.R")

db.times <- readRDS("fullpath.db.timing.rds")
big.times <- db.times[N==max(N)]
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
br.vec <- 10^seq(-2, 3)
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
both.dt <- rbind(
  timing.dt[, .(mean=mean.seconds, sd=sd.seconds, algorithm, grid.fac)],
  big.stats[, .(mean, sd, algorithm, grid.fac)])
text.dt <- both.dt[, .SD[which.max(grid.fac)], by=list(algorithm)]
gg <- ggplot()+
  xlab("Grid points used in approximate computation")+
  scale_y_log10(
    "Time to compute model selection function",
    breaks=br.vec,
    limits=range(br.vec))+
  theme_bw()+
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
png("figure-fullpath-grid-timing.png", 4, 3.2, units="in", res=300)
print(gg)
dev.off()
