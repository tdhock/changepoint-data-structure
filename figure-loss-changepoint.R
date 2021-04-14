library(animint2)
library(data.table)

data(intreg, package="animint2")
signal.colors <- c(estimate="#0adb0a", latent="#0098ef")
breakpoint.colors <- c("1breakpoint"="#ff7d7d", "0breakpoints"='#f6f4bf')
model.linetypes <- c(margin="dotted",limit="dashed",regression="solid")
intreg$annotations$logratio <- max(intreg$sig$log)
## To get the bottom 3 plots to line up properly, we need to plot some
## geom_blanks bigger than the x range, so we calculate that here.
blank.items <- with(intreg,{
  list(segments=list(data=selection,x="min.L",y="segments"),
       error=list(data=selection,x="max.L",y="cost"),
       regression=list(data=model,x=c("min.L","max.L"),
                       y=c("min.feature","max.feature")),
       intervals=list(data=intervals,x=c("min.L","max.L"),y="feature"))
})
Lrange <- c()
for(N in names(blank.items)){
  L <- blank.items[[N]]
  Lrange <- range(c(Lrange,unlist(L$data[,L$x])),finite=TRUE)
  blank.items[[N]]$yrange <- range(unlist(L$data[,L$y]))
}
Lrange[1] <- Lrange[1]-1
Lrange[2] <- Lrange[2]+1
for(N in names(blank.items)){
  L <- blank.items[[N]]
  blank.items[[N]]$blank <- data.frame(x=Lrange, y=L$yrange)
}

pid.chr <- "4.2"
label.dt <- data.table(intreg$ann)[pid.chr==signal]
max.seg <- 5L
seg.dt <- data.table(intreg$seg)[pid.chr==signal & segments<=max.seg]
change.dt <- data.table(intreg$breaks)[pid.chr==signal & segments<=max.seg]
signal.dt <- data.table(intreg$sig)[pid.chr==signal]
opt.fit <- jointseg::Fpsn(signal.dt$logratio, max.seg)
bin.fit <- binseg::binseg_normal(signal.dt$logratio, max.seg)
for(n.seg in 1:max.seg){

}
ggplot()+
  scale_x_continuous("position on chromosome (mega base pairs)",
                     breaks=c(100,200))+
  geom_tallrect(aes(xmin=first.base/1e6, xmax=last.base/1e6,
                    fill=annotation),
                data=label.dt)+

  scale_fill_manual(values=breakpoint.colors,guide="none")+
  geom_text(aes((first.base+last.base)/2e6, logratio+1/8,
                label=annotation),
            data=label.dt)+
  geom_point(aes(base/1e6, logratio),
             data=signal.dt)+
  geom_segment(aes(
    first.base/1e6, mean, xend=last.base/1e6, yend=mean),
    data=seg.dt,
    colour=signal.colors[["estimate"]])+
  geom_vline(aes(
    xintercept=base/1e6),
    colour=signal.colors[["estimate"]],
    linetype="dashed",
    data=change.dt)+
  facet_grid(segments~., scales="free", space="free_x")+
  theme_bw()+
  theme(panel.margin=grid::unit(0,"cm"))



