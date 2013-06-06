library(raster)

##source("F:/GIT/tripEstimation_resource/miscellaneous/Pimage.R")
source("~/Documents/GIT/tripEstimation_resource/miscellaneous/Pimage.R")

load("C:\\Users\\michae_sum\\Documents\\GIT\\tripEstimation_datacache\\SBW_\\SBW_Movement_20Krun.Rdata")


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





pZ <- bin.chain(fit)

ct <- cut(as.POSIXct(pZ), "1 week")
for (il in levels(ct)) {
    image(pZ[ct == il], col = oc.colors(26));scan("", 1)

}


## could hide this with a default for input model object to bin()
grid <- .chaingrid(fit$z)
p <- Pimage(fit$model$twilight, grid)

pZ <- bin(p, fit$z)

library(trip)
source("C:\\Users\\michae_sum\\Documents\\GIT\\tripEstimation_resource\\miscellaneous\\mostlikelypositions.R")


x <- mlp(pZ)

