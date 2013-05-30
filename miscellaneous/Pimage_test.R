library(raster)

source("F:/GIT/tripEstimation_resource/miscellaneous/Pimage.R")

r <- raster()

Pimage(Sys.time() + sort(runif(10, 1, 1000)), r)
