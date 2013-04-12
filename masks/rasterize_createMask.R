########
#######
## raster mask from built-in polygons

library(raster)
library(maptools)

## load in the world polygons
data(wrld_simpl)

## define a region of interest
xlim <- c(-160, -100)
ylim <- c(10, 50)

## build a raster with a given dimension
r <- raster(nrows = 250, ncols = 250, xmn = xlim[1], xmx = xlim[2],
            ymn = ylim[1], ymx = ylim[2], crs = proj4string(wrld_simpl))

## populate the raster with values from the polygons
r <- rasterize(wrld_simpl, r)

## process the values in order to be a mask
r <- is.na(r)  ## sea is now 1

## degrade this raster to an R list with components x, y, z which is
## what tripEstimation expects (tripEstimation could be trained to do this)
mask <- as.image.SpatialGridDataFrame(as(r, "SpatialGridDataFrame"))

## if you want the land and not the ocean :)
mask$z <- !mask$z

## now create the lookup function
library(tripEstimation)
lookup <- mkLookup(mask, by.segment = FALSE)

## convinced?

##image(mask, main = "click on the map")
##while(TRUE) {
##    xy <- locator(1)
##    text(xy$x, xy$y, lab = lookup(cbind(xy$x, xy$y)))
##}

########
#######
## raster mask from ETOPO1
library(raster)
library(rgdal)

r <- raster("ETOPO1_Ice_g_geotiff.tif")

## define a region of interest
xlim <- c(140, 180)
ylim <- c(-50, -20)

## crop the raster
r <- crop(r, extent(xlim[1], xlim[2], ylim[1], ylim[2]))

## process the logic of the mask
r <- r < 500 & r > -2000

## degrade this raster to an R list with components x, y, z which is
## what tripEstimation expects (tripEstimation could be trained to do this)
mask <- as.image.SpatialGridDataFrame(as(r, "SpatialGridDataFrame"))

## if you want the land and not the ocean :)
##mask$z <- !mask$z

## now create the lookup function
library(tripEstimation)
lookup <- mkLookup(mask, by.segment = FALSE)

