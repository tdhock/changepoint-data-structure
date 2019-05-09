source("packages.R")

data(neuroblastomaProcessed, package="penaltyLearning")

err.dt <- data.table(neuroblastomaProcessed$errors)


log.pen.vec <- err.dt[, c(max.log.lambda, min.log.lambda)]
finite.pen.vec <- log.pen.vec[is.finite(log.pen.vec)]
pen.range.vec <- c(
  floor(min(finite.pen.vec)),
  ceiling(max(finite.pen.vec)))

ref.dt <- err.dt[, .(profile.id, chromosome, errors, min.lambda, max.lambda)]
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
    problem.vars=c("profile.id", "chromosome"))
  approx.target.list[[paste(n.grid)]] <- data.table(
    n.grid, target.dt)
}
approx.target <- do.call(rbind, approx.target.list)

saveRDS(approx.target, "neuroblastoma.grid.rds")
