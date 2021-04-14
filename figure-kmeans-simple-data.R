source("packages.R")
pre <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
f <- "zip.test.gz"
if(!file.exists(f)){
  u <- paste0(pre, f)
  download.file(u, f)
}
zip.dt <- data.table::fread(f)
zip.mat <- as.matrix(zip.dt[,-1])
kmeans.dt.list <- list()
for(n.clusters in 1:200){
  print(n.clusters)
  set.seed(1)
  fit <- kmeans(zip.mat, n.clusters)
  kmeans.dt.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    loss=fit$tot.withinss)
}
(kmeans.dt <- do.call(rbind, kmeans.dt.list))

data.table::fwrite(kmeans.dt, "figure-kmeans-simple-data.csv")
