source("packages.R")
compare.list <- readRDS("figure-crops-compare-data.rds")
selection.dt <- compare.list$loss[, {
  penaltyLearning::modelSelection(.SD, "rss", "N.segs")
}, by=.(meta.i,profile.id,chromosome,N.data,algorithm)]
loss.wide <- data.table::dcast(selection.dt, meta.i + profile.id + chromosome + N.data + N.segs ~ algorithm, value.var="rss")
loss.wide[, loss.diff := PDPA - CROPS]
loss.wide[PDPA < CROPS]
PDPA.worse <- loss.wide[PDPA > CROPS]
nrow(PDPA.worse)
PDPA.worse[, .(
  models=.N,
  max.diff=max(loss.diff),
  min.segs=min(N.segs),
  max.segs=max(N.segs)
), by=.(meta.i, profile.id, chromosome, N.data)]
loss.wide[is.na(PDPA)]
loss.wide[is.na(CROPS)]

ggplot()+
  geom_point(aes(
    N.data, seconds, color=expr),
    data=compare.list$timing)+
  scale_x_log10()+
  scale_y_log10()

timing.stats <- compare.list$timing[, .(
  max=max(seconds),
  median=median(seconds),
  min=min(seconds)
), by=.(algorithm=expr, N.data)]
gg <- ggplot()+
  geom_line(aes(
    N.data, median, color=algorithm),
    data=timing.stats)+
  geom_ribbon(aes(
    N.data, ymax=max, ymin=min, fill=algorithm),
    alpha=0.5,
    data=timing.stats)+
  timing.stats[, scale_x_log10(
    breaks=c(min(N.data), 10, 100, 1000, max(N.data))
  )]+
  coord_cartesian(xlim=c(2, 50000))+
  scale_y_log10("Seconds of computation time
(min/median/max over three timings)")
dl <- directlabels::direct.label(gg, "right.polygons")
png("figure-crops-compare.png", height=3, width=4, res=200, units="in")
print(dl)
dev.off()
