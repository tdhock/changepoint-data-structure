source("packages.R")
pre <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
f <- "zip.test.gz"
if(!file.exists(f)){
  u <- paste0(pre, f)
  download.file(u, f)
}
zip.dt <- data.table::fread(f)
zip.mat <- as.matrix(zip.dt[,-1])
N <- nrow(zip.mat)
some.zip <- zip.mat[1:N,]
fit <- prcomp(some.zip)

pred.x <- matrix(colMeans(some.zip), nrow=N, ncol=ncol(some.zip), byrow=TRUE)
pred.x[1:4, 1:4]
loss.vec <- numeric()
saveLoss <- function(idx){
  loss.vec[[paste(idx)]] <<- sum((pred.x - some.zip)^2)
}
saveLoss(0)
for(i in 1:ncol(some.zip)){
  print(i)
  l <- matrix(fit$x[,i], nrow=N, ncol=ncol(some.zip), byrow=FALSE)
  r <- matrix(fit$rotation[,i], nrow=N, ncol=ncol(some.zip), byrow=TRUE)
  pred.x <- pred.x + l*r
  saveLoss(i)
}

plot(loss.vec)
plot(fit[["sdev"]])

pca.dt <- data.table(loss=loss.vec, components=as.integer(names(loss.vec)))
data.table::fwrite(pca.dt, "figure-pca-simple-data.csv")

