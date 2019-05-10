source("packages.R")

cv <- readRDS("chipseq.cv.rds")

cv[, accuracy.percent := 100-error.percent]
cv.tall <- melt(cv, measure.vars=c("accuracy.percent", "auc"))
ggplot()+
  geom_line(aes(
    n.grid, value),
    data=cv.tall[is.trivial==FALSE])+
  facet_grid(variable ~ ., scales="free")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_x_log10()

