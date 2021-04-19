source("packages.R")

N.vec <- c(10, 100, 1000, 1e4)
set.seed(1)
one.signal <- data.table(logratio=rnorm(max(N.vec)))
binseg.timing.list <- list()
for(N in N.vec){
  print(N)
  x <- one.signal$logratio[1:N]
  binseg.timing.list[[paste(N)]] <- data.table(
    N, microbenchmark::microbenchmark(
      multiBinSeg=fpop::multiBinSeg(x, N-1),
      binseg_normal={
        .C(
          "binseg_normal_interface",
          data.vec=as.double(x),
          size=as.integer(length(x)),
          Kmax=as.integer(length(x)),
          end.vec=integer(length(x)),
          cost=double(length(x)),
          before.mean=double(length(x)),
          after.mean=double(length(x)),
          before.size=integer(length(x)),
          after.size=integer(length(x)),
          invalidates.index=integer(length(x)),
          invalidates.after=integer(length(x)),
          PACKAGE="binseg")
      },
      binseg_normal_cost={
        .C(
          "binseg_normal_cost_interface",
          data.vec=as.double(x),
          size=as.integer(length(x)),
          Kmax=as.integer(length(x)),
          cost=double(length(x)),
          PACKAGE="binseg")
      },
      times=5)
  )
}

binseg.timing <- do.call(rbind, binseg.timing.list)
binseg.timing[, seconds := time /1e9]
binseg.stats <- binseg.timing[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N)]
gg <- ggplot()+
  geom_line(aes(
    N, mean, color=expr),
    data=binseg.stats)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=binseg.stats)+
  scale_x_log10(limits=c(NA, max(binseg.stats$N)*10))+
  scale_y_log10()
directlabels::direct.label(gg, "last.polygons")
## binseg_normal_cost is fastest, and others are slower by constant
## factors, as expected.

selected.list <- list()
constant <- function(x)x*0
real <- function(i)one.signal$logratio[i]
sin.linear <- function(i)sin(i)+i
for(fun.name in c("sin", "identity", "constant", "real", "sin.linear")){
  for(N in 10^seq(1, 4)){
    fun <- get(fun.name)
    x <- fun(1:N)#+rnorm(N, 0, 0.01)
    L <- .C(
      "binseg_normal_interface",
      data.vec=as.double(x),
      size=as.integer(length(x)),
      Kmax=as.integer(length(x)),
      end.vec=integer(length(x)),
      cost=double(length(x)),
      before.mean=double(length(x)),
      after.mean=double(length(x)),
      before.size=integer(length(x)),
      after.size=integer(length(x)),
      invalidates.index=integer(length(x)),
      invalidates.after=integer(length(x)),
      PACKAGE="binseg")
    path <- penaltyLearning::modelSelection(data.table(
      loss=L$cost,
      complexity=1:N))
    selected.list[[paste(fun.name, N)]] <- data.table(
      fun.name, N, models=nrow(path),
      mean.max.it=with(L, mean(ifelse(
        before.size<after.size, after.size, before.size))))
  }
}
(selected <- do.call(rbind, selected.list))
## sin function has number of selected models which is linear in N.

timing.dt.list <- list()

N.vec <- 10^seq(2, 5.5, by=0.5)
new.N <- N.vec[! N.vec %in% names(timing.dt.list)]
##new.N <- N.vec
for(N in new.N){
  ##sub.dt <- one.dt[1:N]
  print(N)
  ##x <- rnorm(N)
  ##x <- one.signal$logratio[1:N]
  i <- 1:N
  x <- sin(i)+i/N
  binseg.loss <- function(){
    loss <- .C(
      "binseg_normal_cost_interface",
      data.vec=as.double(x),
      size=as.integer(length(x)),
      Kmax=as.integer(length(x)),
      cost=double(length(x)),
      PACKAGE="binseg")$cost
    loss[c(TRUE, diff(loss) < 0)]
  }
  complexity.vec <- 1:N
  loss.vec <- binseg.loss()
  timing.dt.list[[paste(N)]] <- data.table(N, microbenchmark(
    binseg=binseg.loss(),
    binseg.linear={
      loss.vec <- binseg.loss()
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
    binseg.quadratic={
      loss.vec <- binseg.loss()
      .C(
        "modelSelectionQuadratic_interface",
        loss=as.double(loss.vec),
        complexity=as.double(complexity.vec),
        N=as.integer(N),
        models=integer(N),
        breaks=double(N),
        PACKAGE="penaltyLearning")
    },
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
    quadratic={
      .C(
        "modelSelectionQuadratic_interface",
        loss=as.double(loss.vec),
        complexity=as.double(complexity.vec),
        N=as.integer(N),
        models=integer(N),
        breaks=double(N),
        PACKAGE="penaltyLearning")
    },
    times=5))
}
timing.dt <- do.call(rbind, timing.dt.list)

library(ggplot2)
timing.dt[, seconds := time /1e9]
ggplot()+
  geom_point(aes(
    N, seconds, color=expr),
    data=timing.dt)

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
  scale_x_log10(limits=c(NA, max(stats.dt$N)*10))+
  scale_y_log10()
directlabels::direct.label(gg, "last.polygons")

saveRDS(timing.dt, "fullpath.db.binseg.rds")
