source("packages.R")

cv <- readRDS("cv.rds")
cv[, accuracy.percent := 100-error.percent]

cv.tall <- melt(cv, measure.vars=c("auc", "accuracy.percent"))

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(variable ~ ., scales="free")+
  geom_point(aes(
    n.grid, value),
    shape=1,
    data=cv.tall)+
  scale_x_log10(
    "grid points",
    breaks=unique(cv$n.grid))

cv.stats <- cv.tall[, list(
  median=median(value),
  mean=mean(value),
  sd=sd(value),
  q25=quantile(value, 0.25),
  q75=quantile(value, 0.75)
  ), by=list(n.grid, variable)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(variable ~ ., scales="free")+
  geom_ribbon(aes(
    n.grid, ymin=mean-sd, ymax=mean+sd),
    alpha=0.5,
    data=cv.stats)+
  ## geom_ribbon(aes(
  ##   n.grid, ymin=q25, ymax=q75),
  ##   alpha=0.5,
  ##   data=cv.stats)+
  geom_line(aes(
    n.grid, mean),
    data=cv.stats)+
  scale_x_log10(
    "grid points",
    breaks=unique(cv$n.grid))
