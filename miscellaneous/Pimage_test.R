library(raster)

##source("F:/GIT/tripEstimation_resource/miscellaneous/Pimage.R")
source("~/Documents/GIT/tripEstimation_resource/miscellaneous/Pimage.R")

## creation
r <- raster()
times <- Sys.time() + sort(runif(10, 1, 1000))
p <- Pimage(times, r)

r <- raster(volcano)
p <- Pimage(times, r)

print(p)
summary(p)
.times(p)
.Z(p)


