source("packages.R")

loss.vec <- 10:1 # both linear.
loss.vec <- (10:1)^2 # both quadratic.
(loss.vec <- (10:1)^2 + c(rep(100, 9), 0)) #Rigaill linear, sometimes quadratic.
complexity.vec <- seq_along(loss.vec)
n.input <- length(loss.vec)
iterations.dt.list <- list()
for(algo in c("Rigaill", "QuadraticSometimes")){
  interface <- paste0("modelSelection", algo, "_interface")
  L <- .C(
    interface,
    loss=as.double(loss.vec),
    complexity=as.double(complexity.vec),
    n.input=as.integer(n.input),
    models=integer(n.input),
    breaks=double(n.input),
    iterations=integer(n.input),
    PACKAGE="penaltyLearning")
  iterations.dt.list[[algo]] <- with(L, data.table(
    algo, iterations))
}
(iterations.dt <- do.call(rbind, iterations.dt.list))

saveRDS(timing.dt, "diff.quadratic.rigaill.rds")
