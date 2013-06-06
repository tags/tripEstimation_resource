## Pimage

## creation is hidden, behind bin(), but Pimage() does that work
##   x <- bin(chain)  ## this is enough, but grid argument to customize the grid details
##   a Pimage is a list of child windows, and appears to the user as
##   a 3D array of sorts, with x/y/time
##   - none of the details need to be known to the user, though the Pimage object can be worked with in 
##     the following ways
##   raster(x, time.subset = length(x)) ## can be any index from 1:length(x)
##   x[], x[1:length(x)]   # same as above
##   x["weeks"] ## one of "days", "weeks", "months", "quarters", "years"
##      this serves to break the Pimage time series into these durations based on the time index, and returns a
##     3D raster object, with the formatted date-times as the layers names, or as metadata 

##    the elements should always be trimmed internally, to avoid padding of 0
##     raster also needs an option for trim() that takes a value


## ??  should indexing prevent duplicates? warning



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

