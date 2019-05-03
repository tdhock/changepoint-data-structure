data("neuroblastoma", package="neuroblastoma")
one <- subset(neuroblastoma$profiles, profile.id=="583" & chromosome=="3")
for(i in 1:10000){
  print(i)
  fit <- fpop::multiBinSeg(one$logratio, nrow(one))
}
