library(data.table)
library(penaltyLearning)
library(microbenchmark)
timing.dt.list <- list()
for(N in 10^seq(1, 5, by=0.5)){
  N.chr <- paste(N)
  if(! N.chr %in% names(timing.dt.list)){
    x <- rnorm(N)
    m <- mean(x)
    L1 <- sum((x-m)^2)
    binseg <- function(){
      fit <- fpop::multiBinSeg(x, N-1)
      cumsum(c(L1, fit$J.est))
    }
    loss.vec <- binseg()
    complexity.vec <- 1:N
    print(N)
    timing.dt.list[[paste(N)]] <- data.table(N, microbenchmark(
      binseg=binseg(),
      sort=sort(x),
      linear={
        .C(
          "modelSelectionFwd_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          N=as.integer(N),
          models=integer(N),
          breaks=double(N),
          evals=integer(N),
          PACKAGE="penaltyLearning")
      },
      binseg.linear={
        loss.vec <- binseg()
        .C(
          "modelSelectionFwd_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          N=as.integer(N),
          models=integer(N),
          breaks=double(N),
          evals=integer(N),
          PACKAGE="penaltyLearning")
      }, binseg.quadratic={
        loss.vec <- binseg()
        .C(
          "modelSelectionQuadratic_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          N=as.integer(N),
          models=integer(N),
          breaks=double(N),
          PACKAGE="penaltyLearning")
      }, quadratic={
        .C(
          "modelSelectionQuadratic_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          N=as.integer(N),
          models=integer(N),
          breaks=double(N),
          PACKAGE="penaltyLearning")
      }, times=5))
  }
}
timing.dt <- do.call(rbind, timing.dt.list)

library(ggplot2)
timing.dt[, seconds := time /1e9]
ggplot()+
  geom_point(aes(
    N, seconds, color=expr),
    data=timing.dt[expr %in% c("linear", "quadratic")])

stats.dt <- timing.dt[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N)]
gg <- ggplot()+
  geom_line(aes(
    N, mean, color=expr),
    data=stats.dt)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=stats.dt)+
  scale_x_log10(limits=c(NA, N*10))+
  scale_y_log10()
directlabels::direct.label(gg, "last.polygons")
