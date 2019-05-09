source("packages.R")

nb.grid <- readRDS("neuroblastoma.grid.rds")
fold.dt <- nb.grid[n.grid==min(n.grid), .(profile.id, chromosome)]
n.folds <- 10
set.seed(1)
fold.dt[, fold := sample(rep(1:n.folds, l=.N))]

data(neuroblastomaProcessed, package="penaltyLearning")
err.dt <- data.table(neuroblastomaProcessed$errors)
err.dt[, id := paste0(profile.id, ".", chromosome)]

match.mat <- namedCapture::str_match_variable(
  rownames(neuroblastomaProcessed$target.mat),
  pid="[0-9]+",
  "[.]",
  chr="[0-9]+")

full.grid <- rbind(
  nb.grid,
  data.table(
    n.grid=Inf,
    profile.id=match.mat[, "pid"],
    chromosome=match.mat[, "chr"],
    min.log.lambda=neuroblastomaProcessed$target.mat[, "min.L"],
    max.log.lambda=neuroblastomaProcessed$target.mat[, "max.L"],
    errors=0))[fold.dt, on=list(profile.id, chromosome)]

future::plan("multiprocess")

cv.list <- list()
for(test.fold in 1:n.folds){
  cat(sprintf("%d / %d folds\n", test.fold, n.folds))
  train.dt <- full.grid[fold != test.fold]
  train.dt[, id := paste0(profile.id, ".", chromosome)]
  test.ids <- fold.dt[fold == test.fold, paste0(profile.id, ".", chromosome)]
  train.ids <- fold.dt[fold != test.fold, paste0(profile.id, ".", chromosome)]
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
