source("packages.R")

random_set_vec  <- structure(function(N, props.vec){
  stopifnot(is.integer(N), length(N)==1, 0<N)
  stopifnot(
    is.numeric(props.vec),
    0 < props.vec, props.vec < 1,
    sum(props.vec)==1)
  not.first.counts <- floor(props.vec[-1]*(N+1))
  ## need to subtract away all but one from the total to get an
  ## optimal answer in the case of ties / multiple modes. e.g. N=5
  ## with 50% train/ 50% test.
  count.vec <- c(N-sum(not.first.counts), not.first.counts)
  set.vec <- rep(names(props.vec), count.vec)
  sample(set.vec)
}, ex=function(){
  library(data.table)
  library(ggplot2)
  tvt.props <- c(test=0.19, train=0.67, validation=0.14)
  tvt.N <- 1234567L
  system.time({
    tvt.vec <- random_set_vec(tvt.N, tvt.props)
  })
  table(tvt.vec, useNA="ifany")/tvt.N
  random_set_vec(6L, c(train=2/3, test=1/3))
  random_set_vec(5L, c(train=2/3, test=1/3))
  random_set_vec(4L, c(train=2/3, test=1/3))
  random_set_vec(3L, c(train=2/3, test=1/3))
  test.rev <- function(N, prop.vec, expected.vec){
    result <- list()
    for(fun.name in c("identity", "rev")){
      fun <- get(fun.name)
      ctab <- table(random_set_vec(N, fun(prop.vec)))
      result[[fun.name]] <- ctab
    }
    result$same <- sapply(
      result, function(tab)identical(as.numeric(tab), expected.vec))
    result
  }
  test.rev(4L, c(test=1/3, train=2/3), c(1, 3))
  table(random_set_vec(3L, c(test=0.5, train=0.5)))
  table(random_set_vec(3L, c(train=0.5, test=0.5)))
  test.rev(3L, c(test=0.4, train=0.6), c(1, 2))
  test.rev(3L, c(test=0.49, train=0.51), c(1, 2))
  test.rev(3L, c(test=0.6, train=0.4), c(2, 1))
  ## 2 is optimal after prob=2/3.
  test.rev(2L, c(test=0.6, train=0.4), c(1, 1))
  test.rev(2L, c(test=0.7, train=0.3), c(2))
  ## visualize the likelihood as a function of the proportion of
  ## success.
  test.prop <- seq(0, 1, by=0.01)
  prob.dt.list <- list()
  n.total <- 2
  for(n.test in 0:n.total){
    prob.dt.list[[paste(n.test)]] <- data.table(
      n.test,
      test.prop,
      prob=dbinom(n.test, n.total, test.prop))
  }
  prob.dt <- do.call(rbind, prob.dt.list)
  thresh.dt <- data.table(thresh=(1:2)/3)
  gg <- ggplot()+
    geom_vline(aes(xintercept=thresh), data=thresh.dt)+
    geom_line(aes(
      test.prop, prob, color=n.test, group=n.test),
      data=prob.dt)
  directlabels::direct.label(gg, "last.polygons")
  ## visualize the binomial likelihood as a function of number of
  ## successes, for a given probability of success.
  n.total <- 43
  n.success <- 0:n.total
  p.success <- 0.6
  lik.dt <- data.table(
    n.success,
    prob=dbinom(n.success, n.total, p.success))
  ggplot()+
    geom_point(aes(
      n.success, prob),
      data=lik.dt)+
    geom_vline(xintercept=(n.total+1)*p.success)
  ## visualize the multinomial likelihood as a function of number of
  ## successes, for a given probability of success.
  n.total <- 43
  prob.vec <- c(train=0.6, validation=0.3, test=0.1)
  train.dt <- data.table(train=0:n.total)
  grid.dt <- train.dt[, data.table(
    validation=0:(n.total-train)), by=train]
  grid.dt[, prob := dmultinom(
    c(train, validation, n.total-train-validation),
    n.total,
    prob.vec),
    by=.(train, validation)]
  train.bound <- (n.total+1)*prob.vec[["train"]]
  validation.bound <- (n.total+1)*prob.vec[["validation"]]
  guess.dt <- data.table(
    train=floor(train.bound),
    validation=floor(validation.bound))
  max.dt <- grid.dt[which.max(prob)]#same
  max.dt[, test := n.total-train-validation]
  test.rev(
    as.integer(n.total),
    prob.vec,
    max.dt[, c(test, train, validation)])
  ggplot()+
    geom_tile(aes(
      train, validation, fill=prob),
      data=grid.dt)+
    scale_fill_gradient(low="white", high="red")+
    theme_bw()+
    geom_vline(
      xintercept=train.bound)+
    geom_hline(
      yintercept=validation.bound)+
    geom_point(aes(
      train, validation),
      shape=1,
      data=guess.dt)+
    coord_equal()    
  ## visualize what happens when we start obs.seq variable above at 1
  ## or 0. starting at 0 is problematic e.g. 99% train/1% test with
  ## N=2 observations should return 2 train/0 test (and does when
  ## obs.seq starts with 1, but does NOT when obs.seq starts with 0).
  random_set_vec(2L, c(train=0.99, test=0.01))
  obs.dt.list <- list()
  cum.dt.list <- list()
  for(tvt.N in 2:4){
    obs.dt.list[[paste(tvt.N)]] <- data.table(tvt.N, rbind(
      data.table(start=0, obs=seq(0, tvt.N, l=tvt.N)),
      data.table(start=1, obs=seq(1, tvt.N, l=tvt.N))))
    not.round <- data.table(
      set=c("train", "test"),
      cum.thresh=tvt.N*c((tvt.N-2)/(tvt.N-1), 1))
    cum.dt.list[[paste(tvt.N)]] <- data.table(tvt.N, rbind(
      data.table(round=FALSE, not.round),
      not.round[, .(round=TRUE, set, cum.thresh=round(cum.thresh))]))
  }
  cum.dt <- do.call(rbind, cum.dt.list)
  obs.dt <- do.call(rbind, obs.dt.list)
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(tvt.N ~ .)+
    geom_point(aes(
      obs, start),
      data=obs.dt)+
    geom_vline(aes(
      xintercept=cum.thresh, color=round, linetype=round),
      data=cum.dt)
})

pre <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
f <- "spam.data"
if(!file.exists(f)){
  u <- paste0(pre, f)
  download.file(u, f)
}

spam.dt <- data.table::fread(f)
y.col <- 58
y <- spam.dt[[y.col]]
X <- as.matrix(spam.dt[, -y.col, with=FALSE])

forward_selection <- function(set.vec, grad.set, max.features=ncol(X)){
  input.vec <- c()
  path.dt.list <- list()
  all.f <- formula(paste0("~", paste(colnames(X), collapse="+")))
  while(length(input.vec) <= max.features){
    input.str <- paste(input.vec, collapse="+")
    input.rhs <- ifelse(input.str=="", 1, input.str)
    fit.form <- formula(paste("V58~",input.rhs))
    fit <- glm(fit.form, family="binomial", data=spam.dt[set.vec==grad.set])
    set.err <- data.table(
      set=set.vec,
      pred=predict(fit, spam.dt),
      label=ifelse(y==1, 1, -1)
    )[, .(
      loss=mean(log(1+exp(-label*pred))),
      error.percent=100*mean(ifelse(pred>0, 1, -1)!=label)
    ), by=set]
    cat(sprintf("%2d / %2d features\n", length(input.vec), ncol(X)))
    print(set.err)
    model.size <- length(input.vec)
    path.dt.list[[paste(model.size)]] <- data.table(
      model.size, set.err)
    min.dev.name <- if(length(input.vec) == ncol(X)){
      "dummy"
    }else{
      fit.dev <- add1(fit, all.f)
      some.dev <- data.table(fit.dev, name=rownames(fit.dev))
      some.dev[which.min(Deviance), name]
    }
    input.vec <- c(input.vec, min.dev.name)
  }
  do.call(rbind, path.dt.list)
}

set.seed(1)
set.prop.vec <- c(subtrain=0.4, validation=0.3, test=0.3)
table(three.set.vec <- random_set_vec(nrow(spam.dt), set.prop.vec))
three.path.dt <- forward_selection(three.set.vec, "subtrain")

data.table::fwrite(three.path.dt, "figure-regression-simple-data.csv")

