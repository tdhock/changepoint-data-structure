source("packages.R")

loss.small.evals <- readRDS("loss.small.evals.rds")

ref.dt <- data.table(
  slope=c(2, 1),
  intercept=c(-3, -1))
ggplot()+
  geom_point(aes(
    models.in, total.evals),
    data=loss.small.evals,
    shape=21)+
  coord_equal()+
  theme_bw()+
  geom_abline(aes(
    slope=slope, intercept=intercept),
    data=ref.dt,
    color="grey50")

ggplot()+
  geom_point(aes(
    models.in, max.evals),
    data=loss.small.evals,
    shape=21)+
  theme_bw()

limit.dt.list <- list()
for(N in c(10, seq(100, 500, by=100))){
  t <- 1:N
  dt <- data.table(
    worst.case.loss=N-t,
    best.case.loss=N-sqrt(t),
    complexity=t)
## > dt
##    worst.case.loss best.case.loss complexity
## 1:               4           4.00          1
## 2:               3           3.59          2
## 3:               2           3.27          3
## 4:               1           3.00          4
## 5:               0           2.76          5
## >
  for(loss.col in c("best.case.loss", "worst.case.loss")){
    result <- .C(
      "modelSelectionFwd_interface",
      loss=as.double(dt[[loss.col]]),
      complexity=as.double(dt$complexity),
      N=as.integer(nrow(dt)),
      models=integer(nrow(dt)),
      breaks=double(nrow(dt)),
      evals=integer(nrow(dt)),
      PACKAGE="penaltyLearning")
    limit.dt.list[[paste(N, loss.col)]] <- with(result, data.table(
      loss.col,
      models.in=nrow(dt),
      models.out=N+1,
      max.evals=max(evals),
      total.evals=sum(evals)
    ))
  }
}
limit.dt <- do.call(rbind, limit.dt.list)

both.cols <- c("models.in", "total.evals")
d <- function(data.type, dt){
  data.table(data.type, dt[, both.cols, with=FALSE])
}
set.seed(100)
thresh <- 200
nsamp <- 500
both.dt <- rbind(
  d("neuroblastoma", loss.small.evals[models.in<thresh][sample(nsamp)]),
  d("neuroblastoma", loss.small.evals[models.in>thresh][sample(nsamp)]),
  d("synthetic", limit.dt))
bound.dt <- rbind(
  data.table(latex="\\leq 2N-3", bound="upper", hjust=1, vjust=1, x=425, y=1126),
  data.table(latex="\\geq N-1", bound="lower", hjust=0, vjust=1, x=520, y=490))
bound.color <- "grey50"
break.vec <- seq(250, 1000, by=250)
data.labels <- rbind(
  data.table(
    x=745,
    y=955,
    hjust=0.5,
    vjust=0.5,
    label=sprintf(
      "%d neuroblastoma
data sequences", sum(both.dt$data.type=="neuroblastoma")),
data.type="neuroblastoma"),
data.table(
  x=200, y=150, hjust=0, vjust=1,
  label=paste0("$L_t = N-\\sqrt{t}$, synthetic data achieving lower bound"),
  data.type="synthetic"),
data.table(
  x=-10, y=800, hjust=0, vjust=1,
  label=paste0(
    "$L_t = N-t$,
synthetic data achieving
upper bound"),
data.type="synthetic"))
data.colors <- c(
  neuroblastoma="black",
  synthetic="#A020F0")
gg <- ggplot()+
  geom_text(aes(
    x, y, color=data.type, hjust=hjust, vjust=vjust, label=label),
    data=data.labels)+
  geom_text(aes(
    x, y,
    vjust=vjust,
    hjust=hjust,
    label=sprintf(
      "Theoretical %s bound:
$W_N %s$ iterations", bound, latex)),
    color=bound.color,
data=bound.dt)+
  scale_color_manual(values=data.colors)+
  scale_fill_gradient(low="white", high=data.colors[["neuroblastoma"]])+
  ## geom_bin2d(aes(
  ##   models.in, total.evals),
  ##   data=both.dt[data.type=="neuroblastoma"])+
  ## geom_point(aes(
  ##   models.in, total.evals),
  ##   data=both.dt[data.type=="synthetic"],
  ##   shape=21)+
  geom_point(aes(
    models.in, total.evals, color=data.type),
    data=both.dt,
    shape=21)+
  ##coord_equal()+
  theme_bw()+
  theme(panel.grid.minor=element_blank())+
  geom_abline(aes(
    slope=slope, intercept=intercept),
    data=ref.dt,
    color=bound.color)+
  scale_x_continuous(
    "$N$ = number of models (linear scale)",
    breaks=c(range(both.dt$models.in), break.vec))+
  scale_y_continuous(
    "$W_N$ = number of iterations
(time complexity, linear scale)",
    breaks=c(range(both.dt$total.evals), break.vec))+
  guides(color="none")
print(gg)
tikz("figure-loss-small-evals.tex", width=5.5, height=2.1)
print(gg)
dev.off()
