source("packages.R")

data.list <- list()
for(data.type in c("errors", "features", "targets", "folds")){
  f <- paste0(
    "../feature-learning-benchmark/labeled_problems_", data.type, ".csv")
  data.list[[data.type]] <- fread(f)
}

chipseq.grid <- readRDS("chipseq.grid.rds")
chipseq.grid[, set.name := sub("/.*", "", prob.dir)]
chipseq.grid[, problem := sub(".*/", "", prob.dir)]

full.grid <- rbind(
  chipseq.grid,
  data.list$errors[, data.table(
    n.grid=Inf,
    prob.dir,
    min.log.lambda=min.log.penalty,
    max.log.lambda=max.log.penalty,
    errors,
    set.name=sub("/.*", "", prob.dir),
    problem=sub(".*/", "", prob.dir)
  )])[data.list$folds, on=list(set.name, problem)]

future::plan("multiprocess")

cv.list <- list()
for(test.fold in 1:n.folds){
  cat(sprintf("%d / %d folds\n", test.fold, n.folds))
  train.dt <- full.grid[fold != test.fold]
  train.X <- neuroblastomaProcessed$feature.mat[train.ids,]
  test.X <- neuroblastomaProcessed$feature.mat[test.ids,]
  some.dt <- train.dt#[n.grid==min(n.grid)]#uncomment for testing!
  auc.dt <- some.dt[, {
    print(n.grid)
    y.mat <- cbind(min.log.lambda, max.log.lambda)
    rownames(y.mat) <- id
    fit <- penaltyLearning::IntervalRegressionCV(
      train.X, y.mat[train.ids,])
    pred.dt <- data.table(
      id=rownames(test.X),
      pred.log.lambda=as.numeric(fit$predict(test.X)))
    L <- penaltyLearning::ROChange(
      err.dt, pred.dt, "id")
    with(L, data.table(
      thresholds[threshold=="predicted"],
      auc))
  }, by=list(n.grid)]
  cv.list[[test.fold]] <- data.table(
    test.fold, auc.dt)
}
cv <- do.call(rbind, cv.list)
saveRDS(cv, "cv.rds")
