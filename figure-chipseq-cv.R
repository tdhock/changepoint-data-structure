source("packages.R")

cv <- readRDS("chipseq.cv.rds")

cv[, accuracy.percent := 100-error.percent]
cv.tall <- melt(cv, measure.vars=c("accuracy.percent", "auc"))
ggplot()+
  geom_point(aes(
    n.grid, value),
    data=cv.tall[is.trivial==FALSE])+
  facet_grid(variable ~ set.name, scales="free")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_x_log10()

set.stats <- cv[set.name %in% c("H3K27ac_TDH_some", "H3K27me3_TDH_some", "H3K36me3_TDH_ENCODE", "H3K4me3_PGP_immune"), list(
  mean=mean(accuracy.percent),
  sd=sd(accuracy.percent)
), by=list(n.grid, set.name)]
set.stats[, algorithm := factor(
  ifelse(n.grid==Inf, "Exact_linear", "Approx_grid"),
  c("Exact_linear", "Approx_grid"))]
algo.colors <- c(
  Approx_grid="red",
  Exact_linear="black")
set.stats[, `Data set` := paste0("\n", set.name)]
exact.dt <- set.stats[algorithm=="Exact_linear"]
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
    data=set.stats[algorithm=="Approx_grid"])+
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
  ylab("Percent correctly predicted labels
in 4-fold cross-validation
(mean +/- SD over 4 test folds)")+
  scale_x_log10(
    "Grid points used in approximate computation of model selection function")
print(gg)
png("figure-chipseq-cv.png", 9, 2.5, units="in", res=300)
print(gg)
dev.off()

