## generate a "most likely" trip output with error estimates

mlp <- function(x, trim.prob = 0.05, output.trip = TRUE, tripID = "tripID1") {
  if (!inherits(x, "pimg.list")) stop("input must be a pimg.list")
  if (attr(x, "Z")) warning("running on Z inputs rather than Z")
  n <- length(x)
  
  output <- data.frame(best.x = numeric(n), best.y = numeric(n), 
                       xmin = numeric(n), xmax = numeric(n), 
                       ymin = numeric(n), ymax = numeric(n))
  
  for (i in 1:length(x)) {
    
    xx <- tripEstimation:::as.local.pimg(x[[i]])
    xy <- expand.grid(x = xx$x, y = xx$y)
    ml.i <- which.max(as.vector(xx$z))
    output[i, "best.x"] <- xy[ml.i, 1]
    output[i, "best.y"] <- xy[ml.i, 2]
    if (length(xx$z) == 1L) {
      output[i, "ymin"] <- as.numeric(NA)
      output[i, "ymax"] <- as.numeric(NA)
      output[i, "xmin"] <- as.numeric(NA)
      output[i, "xmax"] <- as.numeric(NA)    
    }  else {
      
      ##xx$z <- xx$z / sum(xx$z)
      cl <- contourLines(xx, levels = quantile(xx$z[xx$z > 0], trim.prob))
      clx <- unlist(lapply(cl, function(cli) cli$x))
      cly <- unlist(lapply(cl, function(cli) cli$y))
      output[i, "ymin"] <- min(cly)
      output[i, "ymax"] <- max(cly)
      output[i, "xmin"] <- min(clx)
      output[i, "xmax"] <- max(clx)
    }
  }
  
  output$times <- attr(x, "times") + ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
  output$id <- tripID
  
  if (output.trip) {
    coordinates(output) <- c("best.x", "best.y")
    return(trip(output, c("times", "id")))
  } else {
    return(output)
  }
}


