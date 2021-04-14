compare.list <- readRDS("figure-crops-compare-data.rds")

loss.wide <- data.table::dcast(compare.list$loss, meta.i + profile.id + chromosome + N.data + N.segs ~ algorithm, value.var="rss")
loss.wide[is.na(PDPA)]
loss.wide[is.na(CROPS)]

gg <- ggplot()+
  geom_point(aes(
    N.data, seconds, color=expr),
    data=timing.dt)+
  scale_x_log10()+
  scale_y_log10()
png("figure-crops-compare.png", height=3, width=4, res=200, units="in")
print(gg)
dev.off()
