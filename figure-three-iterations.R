source("packages.R")

it2 <- data.table(loss=c(7, 4), k=c(1, 2), iteration="$t=2$", t=2)

selection.dt <- penaltyLearning::modelSelection(it2, "loss", "k")

ggplot()+
  geom_point(aes(
    x=min.lambda,
    y=loss+k*min.lambda),
    data=selection.dt)+
  geom_abline(aes(
    slope=k,
    intercept=loss),
    data=it2)

it3 <- rbind(
  it2,
  data.table(
    loss=c(it2$loss, 2), k=seq(1, 3), iteration="$t=3, I_t=2$", t=3),
  data.table(
    loss=c(it2$loss, 0), k=seq(1, 3), iteration="$t=3, I_t=1$", t=3))

selection3 <- it3[, {
  df <- penaltyLearning::modelSelection(.SD, "loss", "k")
  data.table(df, i=nrow(df):1)
}, by=list(iteration)]

pen.vec <- seq(0, 5.5, by=0.1)
min3 <- it3[, {
  selected <- sapply(pen.vec, function(pen){
    k[which.min(loss+k*pen)]
  })
  data.table(penalty=pen.vec, .SD[selected])
}, by=list(iteration)]
min3[, cost := loss+k*penalty]

loss.color <- "deepskyblue"
removed.color <- "#41DD5D"
c("#F7FCF5",#lite green
  "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D",
  "#238B45", "#006D2C",
  "#00441B")#dark green
removed.dt <- selection3[t==2 & i==1, .(min.lambda, k, loss)]
kx <- 5.5
arrow.dt <- rbind(
  data.table(x=kx, y=6, xend=4, yend=10, iteration="$t=2$", k=1, i=1, t=2),
  data.table(x=kx, y=4, xend=2.5, yend=8, iteration="$t=2$", k=2, i=2, t=2),
  data.table(x=kx, y=6, xend=4.5, yend=10.8, iteration="$t=3, I_t=1$", k=1, i=1, t=3),
  data.table(x=kx, y=4, xend=3, yend=7, iteration="$t=3, I_t=1$", k=3, i=2, t=3),
  data.table(x=kx, y=6, xend=4.3, yend=10.5, iteration="$t=3, I_t=2$", k=1, i=1, t=3),
  data.table(x=kx, y=4, xend=2.9, yend=8.6, iteration="$t=3, I_t=2$", k=2, i=2, t=3),
  data.table(x=kx, y=2, xend=1.5, yend=5, iteration="$t=3, I_t=2$", k=3, i=3, t=3))
b21 <- selection3[t==2]$max.lambda[1]
try.dt <- it3[t==3, {
  penalty <- loss[2]-loss[3]
  data.table(
    penalty,
    sign=ifelse(b21 < penalty, "<", ">"),
    cost=penalty*2+loss[2])
}, by=iteration]
gg <- ggplot()+
  geom_line(aes(
    penalty, cost),
    color="grey",
    size=3,
    data=min3)+
  geom_segment(aes(
    x, y, xend=xend, yend=yend),
    arrow=grid::arrow(length=grid::unit(0.05, "in"), type="closed"),
    color="grey",
    data=arrow.dt)+
  geom_text(aes(
    x=min.lambda+0.2,
    y=loss+k*min.lambda,
    label=sprintf("$b_{%d,%d}=%.1f$", t, i, min.lambda)),
    hjust=0, vjust=1,
    data=selection3)+
  geom_abline(aes(
    slope=k,
    intercept=loss),
    data=it3)+
  geom_rect(aes(
    xmin=-Inf, xmax=0,
    ymin=-Inf, ymax=Inf),
    fill="white",
    alpha=0.8,
    data=it3[k==1])+
  ## geom_vline(aes(
  ##   xintercept=min.lambda),
  ##   color="grey",
  ##   data=selection3)+
  geom_text(aes(
    x, y, label=sprintf(
      "$K_{%d,%d}=%d$",
      t, i, k)),
    hjust=0,
    color="grey50",
    data=arrow.dt)+
  geom_point(aes(
    x=min.lambda,
    y=loss+k*min.lambda),
    data=selection3)+
  geom_point(aes(
    x=min.lambda,
    y=loss+k*min.lambda),
    color=removed.color,
    shape=1,
    data=removed.dt)+
  geom_point(aes(
    x=penalty,
    y=cost),
    color=removed.color,
    shape=1,
    data=try.dt)+
  geom_text(aes(
    x=ifelse(sign==">", penalty, penalty-0.5),
    y=cost,
    vjust=ifelse(sign==">", -0.5, 0.5),
    label=sprintf(
      ##"$b_{2,1} %s c(3, 2)$", sign
      "$c(3, 2)$"
    )),
    color=removed.color,
    hjust=1,
    data=try.dt)+
  geom_text(aes(
    min.lambda-0.3, loss+k*min.lambda+0.1, label=label),
    color=removed.color,
    hjust=1,
    vjust=0,
    data=data.table(
      removed.dt,
      iteration=c("$t=2$", "$t=3, I_t=1$", "$t=3, I_t=2$"),
      label=c("$c(2, 1)$", "$b_{2,1}$ removed", "$b_{2,1}$ kept")))+
  geom_text(aes(
    penalty, cost, label=sprintf(
      "$f_%d$", k)),
    data=rbind(
      data.table(penalty=0.5, cost=9, k=1, iteration="$t=2$", hjust=1, vjust=0),
      data.table(penalty=2, cost=4, k=3, iteration="$t=3, I_t=1$", hjust=0, vjust=1),
      data.table(penalty=1.4, cost=4, k=3, iteration="$t=3, I_t=2$", hjust=0, vjust=1),
      data.table(penalty=2, cost=6, k=2, iteration="$t=2$", hjust=0, vjust=1)))+
  geom_text(aes(
    x=Inf, y=Inf, label=sprintf(
      "$b_{%d,0}=\\infty$", t)),
    hjust=1,
    vjust=1,
    data=it3[k==1])+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ iteration)+
  geom_text(aes(
    -0.5, loss, label=sprintf(
    "$L_%d=%.0f$", k, loss)),
    color=loss.color,
    hjust=1,
    data=it3)+
  geom_point(aes(
    0, loss),
    shape=1,
    color=loss.color,
    data=it3)+
  coord_cartesian(
    ylim=c(-1.5, max(min3$cost)),
    xlim=c(-4, 10),
    expand=FALSE)+
  scale_x_continuous(
    "Penalty $\\lambda$",
    breaks=seq(0, 6, by=2))+
  scale_y_continuous(
    "Cost $f_k(\\lambda) = L_k + \\lambda k$",
    breaks=seq(0, 12, by=2))
print(gg)
tikz("figure-three-iterations.tex", 5.5, 2)
print(gg)
dev.off()

