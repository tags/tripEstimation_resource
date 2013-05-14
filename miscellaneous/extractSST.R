extractSST <- function(x, sstfile = "sst.wkmean.1990-present.nc") {
  if (!inherits(x, "trip")) stop("only trip objects supported")
  sst <- brick(sstfile)
  sst.times <- as.POSIXct(strptime(names(sst), "X%Y.%m.%d"))
  time.index <- findInterval(x[[getTORnames(x)[1]]], sst.times)
  values <- numeric(nrow(x))
  
  if (min(coordinates(x)[,1]) < 0) {
    coords <- coordinates(x)
    coords[,1][coords[,1] < 0] <- coords[,1][coords[,1] < 0] + 360
    x <- SpatialPointsDataFrame(coords, data.frame(x = 1:nrow(coords)))
  }
  xy <- coordinates(x)
  for (i in seq_along(time.index)) {
    this.time <- sst[[time.index[i]]]
    values[i] <- extract(this.time, xy[i,,drop = FALSE])
  }
  values
}