source("packages.R")

data.list <- list()
for(data.type in c("possible_errors", "errors", "features", "targets", "folds")){
  f <- paste0(
    "../feature-learning-benchmark/labeled_problems_", data.type, ".csv")
  data.list[[data.type]] <- fread(f)
}
data.list$errors[, max.log.lambda := max.log.penalty]
data.list$errors[, min.log.lambda := min.log.penalty]
err.dt <- with(data.list, possible_errors[errors, on=list(prob.dir)])
err.dt[, possible.fn := possible.tp]

chipseq.grid <- readRDS("chipseq.grid.rds")
chipseq.grid[, set.name := sub("/.*", "", prob.dir)]
chipseq.grid[, problem := sub(".*/", "", prob.dir)]

full.grid <- rbind(
  chipseq.grid,
  data.list$targets[, data.table(
    n.grid=Inf,
    prob.dir,
    min.log.lambda=min.log.penalty,
    max.log.lambda=max.log.penalty,
    errors=NA,
    set.name=sub("/.*", "", prob.dir),
    problem=sub(".*/", "", prob.dir)
  )])[data.list$folds, on=list(set.name, problem)]

future::plan("multiprocess")

test.sets <- unique(full.grid[, .(set.name, fold, n.grid)])
col.name.list <- list(
  features=names(data.list$features)[-1],
  targets=c("min.log.lambda", "max.log.lambda"))

some.sets <- test.sets
##some.sets <- test.sets[set.name=="ATAC_JV_adipose" & fold==1]
OneSet <- function(test.set.i){
  test.set <- some.sets[test.set.i]
  set.grid <- test.set[, .(set.name, n.grid)]
  all.targets <- full.grid[set.grid, on=list(set.name, n.grid)]
  prob.tab <- table(all.targets$prob.dir)
  if(any(1 < prob.tab)){
    stop("there should be only 1 target per problem")
  }
  train.dt.list <- list(
    targets=all.targets[fold != test.set$fold])
  train.dt.list$features <- data.list$features[
    train.dt.list$targets, on=list(prob.dir)]
  keep <- train.dt.list$targets[, is.finite(min.log.lambda) | is.finite(max.log.lambda)]
  train.mat.list <- list()
  for(data.type in names(train.dt.list)){
    col.name.vec <- col.name.list[[data.type]]
    train.mat.list[[data.type]] <- as.matrix(
      train.dt.list[[data.type]][keep, col.name.vec, with=FALSE])
  }
  test.targets <- all.targets[fold == test.set$fold]
  ## TODO filter -Inf, Inf rows
  test.features <- data.list$features[test.targets, on=list(prob.dir)]
  test.X <- as.matrix(test.features[, col.name.list$features, with=FALSE])
  is.trivial <- any(colSums(is.finite(train.mat.list$targets))==0)
  pred.log.lambda <- if(is.trivial){
    rep(0, nrow(test.features))
  }else{
    set.seed(1)
    fit <- with(train.mat.list, penaltyLearning::IntervalRegressionCV(
      features, targets))
    as.numeric(fit$predict(test.X))
  }
  pred.dt <- data.table(
    prob.dir=test.features$prob.dir,
    pred.log.lambda)
  L <- penaltyLearning::ROChange(
    err.dt, pred.dt, "prob.dir")
  auc.dt <- with(L, data.table(
    thresholds[threshold=="predicted"],
    is.trivial,
    auc))
  data.table(test.set, auc.dt)
}
LAPPLY <- future.apply::future_lapply
cv.list <- LAPPLY(1:nrow(some.sets), OneSet)
cv <- do.call(rbind, cv.list)

saveRDS(cv, "chipseq.cv.rds")
