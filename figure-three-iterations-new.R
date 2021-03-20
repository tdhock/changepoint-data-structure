source("packages.R")

p <- function(penalty, loss, k){
  data.table(penalty, loss, k, iteration="$t=2$")
}
inserts <- rbind(
  p(1.0, 0.0, 3),
  p(6.0, 7.0, 1))

it2 <- data.table(loss=c(7, 0), k=c(1, 3), iteration="$t=2$", t=2)

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

i2 <- function(l){
  with(it2, data.table(
    loss=c(loss, l),
    k=c(k, 2),
    iteration=sprintf("$t=3, L_2=%d$", l),
    t=3))
}
it3 <- rbind(it2, i2(2), i2(4))[order(iteration, k)]

selection3 <- it3[, {
  df <- penaltyLearning::modelSelection(.SD, "loss", "k")
  data.table(df, i=nrow(df):1)
}, by=list(iteration)]

max.pen <- 13
pen.vec <- seq(0, max.pen, by=0.1)
min3 <- it3[, {
  selected <- sapply(pen.vec, function(pen){
    which.min(loss+k*pen)
  })
  data.table(penalty=pen.vec, .SD[selected])
}, by=list(iteration)]
min3[, cost := loss+k*penalty]

loss.color <- "deepskyblue"
sure.color <- "red"
c("#F7FCF5",#lite green
  "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D",
  "#238B45", "#006D2C",
  "#00441B")#dark green
removed.dt <- selection3[t==2 & i==1, .(min.lambda, k, loss)]
gg <- ggplot()+
  geom_point(aes(
    penalty, loss+penalty*k),
    color=sure.color,
    size=1.5,
    data=inserts)+
  geom_abline(aes(
    slope=k,
    intercept=loss),
    data=it3)+
  geom_rect(aes(
    xmin=-Inf, xmax=0,
    ymin=-Inf, ymax=Inf),
    fill="white",
    data=it3[k==1])+
  geom_point(aes(
    x=min.lambda,
    y=loss+k*min.lambda),
    data=selection3)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ iteration)+
  geom_text(aes(
    -0.5, loss, label=sprintf(
    "$L_%d=%.0f$", k, loss)),
    color=loss.color,
    hjust=1,
    size=3,
    data=it3)+
  geom_point(aes(
    0, loss),
    shape=1,
    color=loss.color,
    data=it3)+
  coord_cartesian(
    ylim=c(-1.5, 15),
    xlim=c(-4, max.pen),
    expand=FALSE)+
  scale_x_continuous(
    "Penalty $\\lambda$",
    breaks=seq(0, 6, by=2))+
  scale_y_continuous(
    "Cost $f_k(\\lambda) = L_k + \\lambda k$",
    breaks=seq(0, 12, by=2))
print(gg)
tikz("figure-three-iterations-new.tex", width=6.5, height=2.5, standAlone=TRUE)
print(gg)
dev.off()
system("pdflatex figure-three-iterations-new")
