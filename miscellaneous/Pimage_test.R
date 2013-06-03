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



## could hide this with a default for input model object to bin()
grid <- .chaingrid(fit$z)
p <- Pimage(fit$model$twilight, grid)

pZ <- bin(p, fit$z)

library(trip)

x <- mlp(pZ)

ct <- as.POSIXct(levels(cut(x$times, "3 days")), tz = "GMT")
                 
ct <- c(ct, max(ct) + 3 * 24 * 3600)
           
x2 <- cut(x, ct)