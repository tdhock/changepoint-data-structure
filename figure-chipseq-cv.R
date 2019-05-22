source("packages.R")

chipseq.cv <- readRDS("chipseq.cv.rds")
fold.labels <- unique(chipseq.cv[, .(set.name, fold, labels)])
set.dt <- fold.labels[, .(
  total.labels=sum(labels)
), by=list(set.name)][order(total.labels)]
set.dt[, experiment := sub("_.*", "", set.name)]
cv <- set.dt[chipseq.cv, on=list(set.name)]
dstr <- function(experiment, total.labels){
  paste0("\n", experiment, "\nLabels=", total.labels)
}
dfac <- function(experiment, total.labels){
  factor(
    dstr(experiment, total.labels),
    set.dt[, dstr(experiment, total.labels)])
}

some.sets <- c(
  "H3K27ac_TDH_some", "H3K27me3_TDH_some",
  "H3K27ac-H3K4me3_TDHAM_BP",
  "H3K36me3_TDH_ENCODE", "H3K4me3_PGP_immune")
cv[, accuracy.percent := 100-error.percent]
cv[, algorithm := factor(
  ifelse(n.grid==Inf, "Exact\nlinear", "Approx\ngrid"),
  c("Exact\nlinear", "Approx\ngrid"))]
cv[, `Data set` := dfac(experiment, total.labels)]
cv.tall <- melt(cv, measure.vars=c("accuracy.percent", "auc"))
ggplot()+
  geom_point(aes(
    n.grid, value),
    data=cv.tall[is.trivial==FALSE])+
  facet_grid(variable ~ set.name, scales="free")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_x_log10()

acc.wide <- dcast(
  cv,
  set.name + `Data set` + fold ~ n.grid,
  value.var="accuracy.percent")
acc.tall <- melt(
  acc.wide,
  id.vars=c("set.name", "Data set", "fold", "Inf"),
  variable.name="n.grid.fac",
  value.name="grid.percent")
acc.tall[, n.grid := as.integer(paste(n.grid.fac))]
acc.tall[, diff.percent := grid.percent-`Inf`]
ggplot()+
  geom_line(aes(
    n.grid, diff.percent, group=fold),
    data=acc.tall)+
  facet_grid(. ~ `Data set`, labeller=label_both)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_y_continuous(
    "Accuracy difference (percent correct labels),
approximate grid search - exact linear algo
(one line for each test fold in 4-fold CV)")+
  scale_x_log10(
    "Grid points used in approximate computation of model selection function (log scale)")

ggplot()+
  geom_line(aes(
    n.grid, diff.percent, group=fold),
    data=acc.tall[set.name %in% some.sets])+
  facet_grid(. ~ `Data set`, labeller=label_both)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_y_continuous(
    "Accuracy difference (percent correct labels),
approximate grid search - exact linear algo
(one line for each test fold in 4-fold CV)")+
  scale_x_log10(
    "Grid points used in approximate computation of model selection function (log scale)")

diff.stats <- acc.tall[, list(
  mean=mean(diff.percent),
  sd=sd(diff.percent)
), by=list(n.grid, set.name, `Data set`)]
some.diff.stats <- diff.stats[set.name %in% some.sets]
gg.diff <- ggplot()+
  geom_hline(
    yintercept=0,
    color="grey")+
  geom_line(aes(
    n.grid, mean),
    data=some.diff.stats)+
  geom_ribbon(aes(
    n.grid, ymin=mean-sd, ymax=mean+sd),
    alpha=0.5,
    data=some.diff.stats)+
  facet_grid(. ~ `Data set`, labeller=label_both)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_y_continuous(
    "Accuracy diff. (percent correct labels),
Approx. grid search - Exact linear algo
(mean +/- SD over 4 CV folds)")+
  scale_x_log10(
    "Grid points used in approximate computation of model selection function (log scale)")
png("figure-chipseq-cv-diff.png", 10, 2.8, units="in", res=300)
print(gg.diff)
dev.off()

set.stats <- cv[set.name %in% some.sets, list(
  mean=mean(accuracy.percent),
  sd=sd(accuracy.percent)
), by=list(n.grid, set.name, `Data set`, algorithm)]
algo.colors <- c(
  "Approx\ngrid"="red",
  "Exact\nlinear"="black")
exact.dt <- set.stats[algorithm=="Exact\nlinear"]
gg <- ggplot()+
  geom_point(aes(
    n.grid, mean, color=algorithm),
    size=10,
    shape="-",
    data=exact.dt)+
  geom_segment(aes(
    n.grid, mean-sd,
    xend=n.grid, yend=mean+sd,
    color=algorithm),
    size=2,
    data=exact.dt)+
  geom_line(aes(
    n.grid, mean, color=algorithm),
    data=set.stats[algorithm=="Approx\ngrid"])+
  scale_color_manual(values=algo.colors)+
  scale_fill_manual(values=algo.colors)+
  geom_ribbon(aes(
    n.grid, ymin=mean-sd, ymax=mean+sd, fill=algorithm),
    data=set.stats,
    alpha=0.5)+
  facet_grid(. ~ `Data set`, scales="free", labeller=label_both)+
  theme_bw()+
  guides(fill="none")+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_y_continuous("Percent correctly predicted labels
in cross-validation (mean +/- SD
over 4 test folds, linear scale)", limits=c(0, 100), breaks=seq(0, 100, by=20))+
  scale_x_log10(
    "Grid points used in approximate computation of model selection function (log scale)")
print(gg)
png("figure-chipseq-cv.png", 8.5, 2.5, units="in", res=300)
print(gg)
dev.off()

