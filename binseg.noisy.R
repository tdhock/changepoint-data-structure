source("packages.R")

selected.list <- list()
constant <- function(x)0*x #need *x so result is vector not scalar.
sin <- function(x)base::sin(x)
identity <- function(x)x
sin.linear.overN <- function(x)sin(x)+x/N
sin.linear <- function(x)sin(x)+x
pwc <- function(x){
  (seq_along(x)-1) %/% 2
}
fun.name.vec <- c(
  "sin.linear", "pwc")
for(fun.name in fun.name.vec){
  for(N in 10^seq(1, 4)){
    fun <- get(fun.name)
    y <- fun(1:N)
    noise.list <- list(
      noiseless=y,
      noisy=y+rnorm(N, 0, 0.1))
    for(noise.name in names(noise.list)){
      x <- noise.list[[noise.name]]
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
      loss.vec <- with(L, cost[cost==cummin(cost)])
      path <- penaltyLearning::modelSelection(data.table(
        loss=loss.vec,
        complexity=1:N))
      selected.list[[paste(fun.name, N, noise.name)]] <- data.table(
        fun.name, N, noise.name,
        models.in=length(loss.vec), models.out=nrow(path),
        mean.max.it=with(L, mean(ifelse(
          before.size<after.size, after.size, before.size))))
    }
  }
}
(selected <- do.call(rbind, selected.list))
## sin function has number of selected models which is linear in N.

## 1. constant = binseg slow, quad selection fast.

## 2. sin.linear = binseg fast, quad selection slow.

## 3. identity = binseg fast, quad selection fast.

## 4. sin = binseg slow, quad selection slow.

timing.dt.list <- list()
N.vec <- 10^seq(2, 4, by=0.5)
new.N <- N.vec[! N.vec %in% names(timing.dt.list)]
##new.N <- N.vec
for(N in new.N){
  i <- 1:N
  for(fun.name in fun.name.vec){
    sim.fun <- get(fun.name)
    x <- sim.fun(i)+rnorm(N, 0, 0.1)
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
    loss.vec <- binseg.loss()
    complexity.vec <- seq_along(loss.vec)
    n.input <- length(loss.vec)
    micro.df <- microbenchmark(
      binseg=binseg.loss(),
      binseg.linear={
        loss.vec <- binseg.loss()
        .C(
          "modelSelectionLinear_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          evals=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      linear={
        linear <- .C(
          "modelSelectionLinear_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          evals=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      binseg.Rigaill={
        loss.vec <- binseg.loss()
        .C(
          "modelSelectionRigaill_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          iterations=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      Rigaill={
        Rigaill <- .C(
          "modelSelectionRigaill_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          iterations=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      binseg.quadAlways={
        loss.vec <- binseg.loss()
        .C(
          "modelSelectionQuadraticAlways_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          iterations=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      quadAlways={
        quadAlways <- .C(
          "modelSelectionQuadraticAlways_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          iterations=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      binseg.quadSometimes={
        loss.vec <- binseg.loss()
        .C(
          "modelSelectionQuadraticSometimes_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          iterations=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      quadSometimes={
        quadSometimes <- .C(
          "modelSelectionQuadraticSometimes_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          iterations=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      times=5)
    stopifnot(quadAlways$n.input == quadSometimes$n.input)
    stopifnot(linear$n.input == quadAlways$n.input)
    rbind(linear$models, quadAlways$models)
    stopifnot(all.equal(
      with(quadAlways, models[1:(n.input+1)]),
      with(quadSometimes, models[1:(n.input+1)])))
    timing.dt.list[[paste(fun.name, N)]] <- data.table(
      fun.name, N, micro.df)
  }
}
timing.dt <- do.call(rbind, timing.dt.list)

timing.dt[, seconds := time /1e9]
stats.dt <- timing.dt[, list(
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(expr, N, fun.name)]
expr.colors <- c(
  binseg.linear="grey50", linear="black",
  binseg.quadAlways="#A6CEE3", quadAlways="#1F78B4",#blue
  binseg.quadSometimes="#B2DF8A", quadSometimes="#33A02C",#green
  binseg.Rigaill="#FB9A99", Rigaill="#E31A1C",#red
  "#FDBF6F", "#FF7F00",#orange
  binseg="#CAB2D6", "#6A3D9A",#purple
  "#FFFF99", "#B15928")#yellow/brown
leg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  scale_color_manual(values=expr.colors)+
  scale_fill_manual(values=expr.colors)+
  geom_line(aes(
    N, mean, color=expr),
    data=stats.dt)+
  geom_ribbon(aes(
    N, ymin=mean-sd, ymax=mean+sd, fill=expr),
    alpha=0.5,
    data=stats.dt)+
  scale_x_log10(
    "N = number of simulated data (log scale)",
    limits=c(NA, max(stats.dt$N)*10))+
  scale_y_log10(
    "Computation time (seconds, log scale)")
directlabels::direct.label(leg, "last.polygons")
