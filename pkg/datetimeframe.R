library(GeoLight)
data(hoopoe1)

d <- data.frame(gmt = levels(hoopoe1$datetime)[hoopoe1$datetime], light = hoopoe1$light, stringsAsFactors = FALSE)

d <- d[1:50, ]

library(xts)

try.xts(d$gmt)
