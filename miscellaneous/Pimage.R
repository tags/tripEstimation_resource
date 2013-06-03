Pimage <- function(tm, grid = NULL, Z = TRUE) {
    stopifnot(inherits(tm, "POSIXct"))
    stopifnot(inherits(Z, "logical"))
    stopifnot(all(diff(unclass(tm)) > 0))
    stopifnot(length(tm) >= 2)

    p0 <- function (xmin, xmax, ymin, ymax, xydim) {
        res <- list(xbound = c(xmin, xmax, xydim[1L]),
                    ybound = c(ymin, ymax, xydim[2L]),
                    offset = c(1, 1), image = NULL)
        res
    }
    if (is.null(grid)) {
        grid <- raster()
    } else {
        ## this takes an image() list, raster, SGDF/SPxDF, GridTopology(), extent, file
        grid <- raster(grid)
    }
    n <- length(tm) - Z

    dims <- dim(grid)

    pbase <- p0(xmin(grid), xmax(grid), ymin(grid), ymax(grid), dims[1L:2L])
    pim <- vector("list", n)
    for (i in seq_along(pim)) pim[[i]] <- pbase
    ##if (Z) {
        ##Ztimes <- tm[-length(tm)] + diff(unclass(tm))/2
        attr(pim, "times") <- tm
        ##attr(pim, "Xtimes") <- tm
   ## }
   ## else {
    ##    attr(pim, "times") <- tm
   ## }
    attr(pim, "Z") <- Z
    class(pim) <- c("Pimage")
    pim
}

# trim0<- function(x, ...) {
#  
#   x[!x > 0] <- NA
#   trim(x)
# }
.chaingrid <- function(x) {
  xrange <- range(x[,1,])
  yrange <- range(x[,2,])
  raster(nrows = 300, ncols = 300, 
         xmn = xrange[1L],
         xmx = xrange[2L], 
         ymn = yrange[1L], 
         ymx = yrange[2L]
         )
}
## validity
##  see spatstat/R/verifyclass.S for inspiration
##  any or all NULL images?
##  all pimg objects are consistent
##  all offsets are sensible, etc.


print.Pimage <- function(x, ...) {
  ## this needs to know the x/y/time range, and possibly the sizes of all images, whether any are NULL or funny
    ext <- extent(as.raster(x))
    trange <- format(range(attr(x, "times")))
    Z <- .Z(x)
    cat("Class    :", class(x), c("(Primary/X)", "(Intermediate/Z)")[Z + 1], "\nLength    :", length(x),  "\nTemporal Extent :", trange, "\n")
    ##cat("Time Steps   :")
    ##str(attr(x, "times"))
    print(ext)
    invisible(NULL)
}
summary.Pimage <- function(object, ...) {
  summary(getValues(as.raster(object)))
}

as.POSIXct.Pimage <- function(x, tz = "", ...) {
  .times(x)
}
.times <- function(x) {
  UseMethod(".times")
}
.times.Pimage <- function(x) {
    out <- attr(x, "times")
    ##if (.Z(x)) out <- out[-length(out)]
    out
}

## must be generic for replacement method to work
".times<-" <- function(x, value) {
  UseMethod(".times<-")
}
".times<-.Pimage" <- function(x, value) {
  attr(x, "times") <- value
  x
}
.Z <- function(x) {
  UseMethod(".Z")
}
.Z.Pimage <- function(x) {
  attr(x, "Z")
}
".Z<-" <- function(x, value) {
  UseMethod(".Z<-")
}
".Z<-.Pimage" <- function(x, value) {
  attr(x, "Z") <- value
  x
}

is.Pimage <- function(x) {inherits(x, "Pimage")}

"[.Pimage" <- function(x, i, j, drop = TRUE, ...) {
  timeobject <- .times(x)
  
  n <- length(x)
  if(nargs() == 1) n2 <-  n
  if (missing(i)) i <- seq_len(n)
  
  if (all(class(i) == "logical")) {
    n2 <- sum(i)
    i <- which(rep(i, length.out = n2))
  }
  
  class(x) <- NULL
  val <- NextMethod("[")
  ##browser()
  class(val) <- "Pimage"

  .times(val) <- timeobject
  val <- as.image.Pimage(val)
  raster(val)
  
}

  ###str.Pimage
  
plot.Pimage <- function(x, ...) {
  plot(x[seq_along(x)], ...)  
}
###"[<-.Pimage"
##"$.Pimage"
##"$<-.Pimage"
#head.Pimage
#tail.Pimage
#range.Pimage
#scale.Pimage ## divide out iterations?
##with.Pimage
##subset.Pimage
#names.Pimage ## formatted date-times
## "names<-.Pimage"
#rev.Pimage  ## should only be in order
## ?? ifelse.Pimage
#median.Pimage
#quantile.Pimage
## transform.Pimage


## coercion
as.Pimage <- function(x, ...) {
    UseMethod("as.Pimage")
}

as.Pimage.Pimage <- function(x) {
    x
}

as.raster.Pimage <- function(x) {
    ## TODO, patch in the data, like tripEstimation::combine()
    ##  include message on object about its origin
    x <- x[[1]]
    raster(nrows = x$xbound[3L], ncols = x$ybound[3L], xmn = x$xbound[1L], xmx = x$xbound[2L], ymn = x$ybound[1L], ymx = x$ybound[2L])
}

bin <- function(x, ...) UseMethod("bin")

bin.Pimage <- function (pimgs, chain, weights = NULL)
{
  if (is.null(weights) && .Z(pimgs)) {
    weights <- c(diff(unclass(.times(pimgs))/3600))
  }
  if (is.null(weights)) weights <- rep(1, length(pimgs))
  
  
  if (!(length(weights) == length(pimgs))) stop("length of weights do not match length of p-img list")
  if (nrow(chain) != length(pimgs))
    stop("dimensions of chain do not match length of p-img list")
  #dm <- dim(z)
  
  
  for (k in seq_along(weights)) {
    pimgs[[k]] <- bin.pimg(pimgs[[k]], t(chain[k, 1:2, ]), weight = weights[k])
  }
  ## should also have a flag for whether this is initialized/scaled, so iter number is independent
  attr(pimgs, "itersbin") <- attr(pimgs, "itersbin") + dim(chain)[3]
  pimgs
}


`bin.pimg` <-
  function(pimg, xy, weight = 1) {
    
    xbnd <- pimg$xbound
    ybnd <- pimg$ybound
    
    ## Bin the locations into the global image coords
    i <- ceiling(xbnd[3]*(xy[,1]-xbnd[1])/(xbnd[2]-xbnd[1]))
    j <- ceiling(ybnd[3]*(xy[,2]-ybnd[1])/(ybnd[2]-ybnd[1]))
    ## Delete those points outside the global image
    keep <- (i >= 1 & i <= xbnd[3] & j >= 1 & j <= ybnd[3])
    if(any(keep)) {
      i <- i[keep]
      j <- j[keep]
      
      ## Expand image to new size
      if(is.null(pimg$image)) {
        irange <- range(i)
        jrange <- range(j)
        off <- c(irange[1],jrange[1])
        img <- matrix(0,diff(irange)+1,diff(jrange)+1)
      } else {
        irange0 <- pimg$offset[1]+c(0,nrow(pimg$image)-1)
        jrange0 <- pimg$offset[2]+c(0,ncol(pimg$image)-1)
        irange <- range(i,irange0)
        jrange <- range(j,jrange0)
        off <- c(irange[1],jrange[1])
        if(all(irange==irange0) && all(jrange==jrange0)) {
          ## Keep original image
          img <- pimg$image
        } else {
          ## Expand image
          img <- matrix(0,diff(irange)+1,diff(jrange)+1)
          img[(irange0[1]-off[1]+1):(irange0[2]-off[1]+1),
              (jrange0[1]-off[2]+1):(jrange0[2]-off[2]+1)] <- pimg$image
        }
      }
      
      ## Add binned points to new image
      img <- img + weight * tabulate(nrow(img) * (j - off[2]) + i + (1 - off[1]), nbins = prod(dim(img)))
      
      pimg <- list(xbound=xbnd,
                   ybound=ybnd,
                   offset=off,
                   image=img)
      class(pimg) <- c("pimg", "list")
    }
    pimg
  }




as.image.Pimage <-
  function (pimgs)
  {
    ## should have checks elsewhere for these NULLs, do they persist when no mixing?
   ## bad <- unlist(lapply(pimgs, function(x) is.null(x$image)))
    `as.matrix.pimg` <-
      function(x) {
        
        pimg <- x
        img <- matrix(0,pimg$xbound[3],pimg$ybound[3])
        if(!is.null(pimg$image)) {
          off <- pimg$offset
          img[off[1]:(off[1]+nrow(pimg$image)-1),
              off[2]:(off[2]+ncol(pimg$image)-1)] <- pimg$image
        }
        img
      }
    
    `as.image.pimg` <-
      function(pimg) {
        img <- coords.pimg(pimg)
        img$z <- as.matrix.pimg(pimg)
        img
      }
    `coords.pimg` <-
      function(pimg) {
        list(x=seq(pimg$xbound[1],pimg$xbound[2],length=pimg$xbound[3]),
             y=seq(pimg$ybound[1],pimg$ybound[2],length=pimg$ybound[3]))
      }
    
    res <- as.image.pimg(pimgs[[1]])
    if (length(pimgs) == 1)
      return(res)
    for (i in seq_along(pimgs)[-1]) {
      img <- pimgs[[i]]
      Xpos <- img$offset[1]
      Ypos <- img$offset[2]
      Xind <- Xpos:(Xpos + dim(img$image)[1] - 1)
      Yind <- Ypos:(Ypos + dim(img$image)[2] - 1)
      res$z[Xind, Yind] <- res$z[Xind, Yind] + img$image
    }
    res
  }



`bin.pimg` <-
  function(pimg, xy, weight = 1) {
    
    xbnd <- pimg$xbound
    ybnd <- pimg$ybound
    
    ## Bin the locations into the global image coords
    i <- ceiling(xbnd[3]*(xy[,1]-xbnd[1])/(xbnd[2]-xbnd[1]))
    j <- ceiling(ybnd[3]*(xy[,2]-ybnd[1])/(ybnd[2]-ybnd[1]))
    ## Delete those points outside the global image
    keep <- (i >= 1 & i <= xbnd[3] & j >= 1 & j <= ybnd[3])
    if(any(keep)) {
      i <- i[keep]
      j <- j[keep]
      
      ## Expand image to new size
      if(is.null(pimg$image)) {
        irange <- range(i)
        jrange <- range(j)
        off <- c(irange[1],jrange[1])
        img <- matrix(0,diff(irange)+1,diff(jrange)+1)
      } else {
        irange0 <- pimg$offset[1]+c(0,nrow(pimg$image)-1)
        jrange0 <- pimg$offset[2]+c(0,ncol(pimg$image)-1)
        irange <- range(i,irange0)
        jrange <- range(j,jrange0)
        off <- c(irange[1],jrange[1])
        if(all(irange==irange0) && all(jrange==jrange0)) {
          ## Keep original image
          img <- pimg$image
        } else {
          ## Expand image
          img <- matrix(0,diff(irange)+1,diff(jrange)+1)
          img[(irange0[1]-off[1]+1):(irange0[2]-off[1]+1),
              (jrange0[1]-off[2]+1):(jrange0[2]-off[2]+1)] <- pimg$image
        }
      }
      
      ## Add binned points to new image
      img <- img + weight * tabulate(nrow(img) * (j - off[2]) + i + (1 - off[1]), nbins = prod(dim(img)))
      
      pimg <- list(xbound=xbnd,
                   ybound=ybnd,
                   offset=off,
                   image=img)
      class(pimg) <- c("pimg", "list")
    }
    pimg
  }

