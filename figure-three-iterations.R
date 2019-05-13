library(ggplot2)
library(tikzDevice)
if(FALSE){
  install.packages("tikzDevice")
}
library(data.table)

it2 <- data.table(loss=c(3, 2), k=seq(1, 2), iteration="$t=2$", t=2, i=1:2)

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
  data.table(loss=c(it2$loss, 1.5), k=seq(1, 3), iteration="$t=3, I_t=2$", t=3, i=1:3),
  data.table(loss=c(it2$loss, 0.5), k=seq(1, 3), iteration="$t=3, I_t=1$", t=3, i=1:3))

selection3 <- it3[, {
  penaltyLearning::modelSelection(.SD, "loss", "k")
}, by=list(iteration)]

gg <- ggplot()+
  geom_point(aes(
    x=min.lambda,
    y=loss+k*min.lambda),
    data=selection3)+
  geom_text(aes(
    x=min.lambda,
    y=loss+k*min.lambda,
    label=sprintf("$b_{%d,%d}$", t, i)),
    hjust=0, vjust=1,
    data=selection3)+
  geom_abline(aes(
    slope=k,
    intercept=loss),
    data=it3)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ iteration)+
  xlab("Penalty $\\lambda$")+
  ylab("Cost $f_t(\\lambda)$")
print(gg)

##TODO geom_text
tikz("figure-three-iterations.tex", 5.5, 2.5)
print(gg)
dev.off()

