source("packages.R")

data.list <- list()
for(data.type in c("errors", "features", "targets")){
  f <- paste0(
    "../feature-learning-benchmark/labeled_problems_", data.type, ".csv")
  data.list[[data.type]] <- fread(f)
}

log.pen.vec <- data.list$errors[, c(max.log.penalty, min.log.penalty)]
finite.pen.vec <- log.pen.vec[is.finite(log.pen.vec)]
pen.range.vec <- c(
  floor(min(finite.pen.vec)),
  ceiling(max(finite.pen.vec)))

ref.dt <- data.list$errors[, .(
  prob.dir, errors,
  min.lambda=exp(min.log.penalty),
  max.lambda=exp(max.log.penalty)
)]
setkey(ref.dt, min.lambda, max.lambda)

approx.target.list <- list()
n.grid.vec <- 2^seq(1, 10)
for(n.grid in n.grid.vec){
  log.pen.grid.vec <- seq(pen.range.vec[1], pen.range.vec[2], l=n.grid)
  log.mid.vec <- log.pen.grid.vec[-1]-diff(log.pen.grid.vec)/2
  pen.grid.vec <- exp(log.pen.grid.vec)
  grid.dt <- data.table(
    penalty=pen.grid.vec, pen0=pen.grid.vec,
    min.log.lambda=c(-Inf, log.mid.vec),
    max.log.lambda=c(log.mid.vec, Inf))
  setkey(grid.dt, penalty, pen0)
  over.dt <- foverlaps(ref.dt, grid.dt, nomatch=0L)
  target.dt <- penaltyLearning::targetIntervals(
    over.dt,
    problem.vars=c("prob.dir"))
  approx.target.list[[paste(n.grid)]] <- data.table(
    n.grid, target.dt)
}
approx.target <- do.call(rbind, approx.target.list)

saveRDS(approx.target, "chipseq.grid.rds")
