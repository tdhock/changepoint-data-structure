source("packages.R")

selected.list <- list()
constant <- function(x)0*x #need *x so result is vector not scalar.
sin <- function(x)base::sin(x)
identity <- function(x)x
sin.linear.overN <- function(x)sin(x)+x/N
sin.linear <- function(x)sin(x)+x
pwc <- function(x){
  (seq_along(x)-1) %/% 5
}
fun.name.vec <- c(
  "sin", "identity", "constant", "sin.linear", "sin.linear.overN", "pwc")
for(fun.name in fun.name.vec){
  for(N in 10^seq(1, 4)){
    fun <- get(fun.name)
    x <- fun(1:N)+rnorm(N, 0, 0.1)
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

## 1. constant = binseg slow, quad selection fast.

## 2. sin.linear = binseg fast, quad selection slow.

## 3. identity = binseg fast, quad selection fast.

## 4. sin = binseg slow, quad selection slow.

f <- function(fun.name, latex, binseg, n.selected){
  data.table(fun.name, binseg, n.selected, latex=sprintf(
    "$x_i = %s$", latex))
}
sim.fun.dt <- rbind(
  f("constant", "0", "slow", "few"),
  f("sin.linear", "i+\\sin(i)", "fast", "many"),
  f("identity", "i", "fast", "few"),
  f("sin", "\\sin(i)", "slow", "many"))

timing.dt.list <- list()
N.vec <- 10^seq(2, 4, by=0.5)
new.N <- N.vec[! N.vec %in% names(timing.dt.list)]
##new.N <- N.vec
for(N in new.N){
  i <- 1:N
  for(sim.fun.i in 1:nrow(sim.fun.dt)){
    sim.fun.row <- sim.fun.dt[sim.fun.i]
    sim.fun <- get(sim.fun.row$fun.name)
    x <- sim.fun(i)
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
          "modelSelectionFwd_interface",
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
          "modelSelectionFwd_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          evals=integer(n.input),
          PACKAGE="penaltyLearning")
      },
      ## binseg.Rigaill={
      ##   loss.vec <- binseg.loss()
      ##   .C(
      ##     "modelSelectionRigaill_interface",
      ##     loss=as.double(loss.vec),
      ##     complexity=as.double(complexity.vec),
      ##     n.input=as.integer(n.input),
      ##     models=integer(n.input),
      ##     breaks=double(n.input),
      ##     iterations=integer(n.input),
      ##     PACKAGE="penaltyLearning")
      ## },
      ## Rigaill={
      ##   Rigaill <- .C(
      ##     "modelSelectionRigaill_interface",
      ##     loss=as.double(loss.vec),
      ##     complexity=as.double(complexity.vec),
      ##     n.input=as.integer(n.input),
      ##     models=integer(n.input),
      ##     breaks=double(n.input),
      ##     iterations=integer(n.input),
      ##     PACKAGE="penaltyLearning")
      ## },
      ## binseg.quadAlways={
      ##   loss.vec <- binseg.loss()
      ##   .C(
      ##     "modelSelectionQuadraticAlways_interface",
      ##     loss=as.double(loss.vec),
      ##     complexity=as.double(complexity.vec),
      ##     n.input=as.integer(n.input),
      ##     models=integer(n.input),
      ##     breaks=double(n.input),
      ##     iterations=integer(n.input),
      ##     PACKAGE="penaltyLearning")
      ## },
      ## quadAlways={
      ##   quadAlways <- .C(
      ##     "modelSelectionQuadraticAlways_interface",
      ##     loss=as.double(loss.vec),
      ##     complexity=as.double(complexity.vec),
      ##     n.input=as.integer(n.input),
      ##     models=integer(n.input),
      ##     breaks=double(n.input),
      ##     iterations=integer(n.input),
      ##     PACKAGE="penaltyLearning")
      ## },
      binseg.quadSometimes={
        loss.vec <- binseg.loss()
        .C(
          "modelSelectionQuadratic_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          PACKAGE="penaltyLearning")
      },
      quadSometimes={
        quadSometimes <- .C(
          "modelSelectionQuadratic_interface",
          loss=as.double(loss.vec),
          complexity=as.double(complexity.vec),
          n.input=as.integer(n.input),
          models=integer(n.input),
          breaks=double(n.input),
          PACKAGE="penaltyLearning")
      },
      times=5)
    ## stopifnot(quadAlways$n.input == quadSometimes$n.input)
    ## stopifnot(linear$n.input == quadAlways$n.input)
    ## rbind(linear$models, quadAlways$models)
    ## stopifnot(all.equal(
    ##   with(quadAlways, models[1:(n.input+1)]),
    ##   with(quadSometimes, models[1:(n.input+1)])))
    timing.dt.list[[paste(sim.fun.i, N)]] <- data.table(
      N, n.input, sim.fun.row, micro.df)
  }
}
timing.dt <- do.call(rbind, timing.dt.list)

saveRDS(timing.dt, "binseg.quadratic.rigaill.rds")
