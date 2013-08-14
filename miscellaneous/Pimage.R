## to consider

## `print.pimg.list` <-
## `summary.pimg.list`
## `plot.pimg.list`

## why does the image sometimes stay NULL?
## what to do with a 1x1 image?
## how to rescale (div by iters) the pimg back to  probability/time spent?
## how to detect chain that is not active (i.e. we escaped out of *.model before it finished?), and related
## how to accumulate to an existing X/Z summary once it's been rescaled already?



## temporary expt

bin.chain <- function (chainobject, grid = NULL, weights = NULL, Z = TRUE)
{
    ## weights from tm difference if Z
    ## times from model
    ## grid from xrange/yrange chain if NULL

    times <- .times(chainobject)
    if (Z) times <- times[-length(times)]
    n <- length(times)

    if (is.null(weights)) {
        if (Z) weights <- c(diff(unclass(times)/3600)) else weights <- seq_len(n)
    }

    if(Z) chain <- chainobject$z else chain <- chainobject$x

    if (is.null(grid)) grid <- .chaingrid(chain)
    pimgs <- Pimage(times, grid = grid, Z = Z)
  for (k in seq_along(weights)) {
    pimgs[[k]] <- bin.pimg(pimgs[[k]], t(chain[k, 1:2, ]), weight = weights[k])
  }
  ## should also have a flag for whether this is initialized/scaled, so iter number is independent
  attr(pimgs, "itersbin") <- attr(pimgs, "itersbin") + dim(chain)[3]
  pimgs
}

.times.default <- function(x) {
    x$model$twilight
}

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

 trim0<- function(x, ...) {

   x[!x > 0] <- NA
   trim(x)
 }
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
    if (.Z(x)) out <- out[-length(out)]
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

names.Pimage <- function(x) {
    format(as.POSIXct(x))
}
"[.Pimage" <- function(x, i, j, drop = TRUE, ...) {
  timeobject <- .times(x)

  n <- length(x)
  if(nargs() == 1) n2 <-  n
  if (missing(i)) i <- seq_len(n)

  if (all(class(i) == "logical")) {
    n2 <- sum(i)
    i <- which(i)
  }

  if (all(class(i) == "character")) {
      if (length(i) == 1L && i %in% c("days", "weeks", "months", "years")) {
          ct <- cut(as.POSIXct(x), i, start.on.monday = FALSE)
      }
      i <- grep(i, names(x))
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




## to consider

## `print.pimg.list` <-
## `summary.pimg.list`
## `plot.pimg.list`

## why does the image sometimes stay NULL?
## what to do with a 1x1 image?
## how to rescale (div by iters) the pimg back to  probability/time spent?
## how to detect chain that is not active (i.e. we escaped out of *.model before it finished?), and related
## how to accumulate to an existing X/Z summary once it's been rescaled already?








#'Create a collection of probability images, for MCMC binning.
#'
#'Pimage lists.
#'
#'
#'@aliases pimg.list
#'@usage pimg.list(times, xlim, ylim, img.dim, Z = TRUE)

#'@param times \code{times}
#'@param xlim \code{xlim}
#'@param ylim \code{ylim}
#'@param img.dim \code{img.dim}
#'@param Z \code{Z}
#'@examples
#'\dontrun{
#'
#'## summarize as if we have run a solar/estelle.model
#'
#'load("F:/GIT/tripEstimation_datacache/SBW_/SBW_Movement_20Krun.Rdata")
#'## load("~/Documents/GIT/tripEstimation_datacache/SBW_/SBW_Movement_20Krun.Rdata")
#'bb <- t(apply(fit$z, 2, range))
#'pZ <- pimg.list(times = fit$model$twilight, xlim = bb[1,], ylim = bb[2,], img.dim = c(100, 80), Z = TRUE)
#'pZ <- behav.bin(fit$z, pZ)
#'pZ1 <- tripBridge:::scale.pimg.list(pZ)
#'image(as.image.pimg.list(pZ), col = rainbow(256))
#'require(raster)
#'plot(raster(as.image.pimg.list(pZ1)), col = rainbow(256))
#'
#'
#' conversion to a trip object, with quantile ellipse
#'library(car)

##'mkEllipse
##'function(xy,...) {
##'  Xbar <- apply(xy, 2, mean)
##'  S <- cov(xy)
## ' ellipse(Xbar, S, 1, col = "black", lwd = 1) }

##'image(tripBridge:::as.image.pimg.list(pZ), col = rainbow(256))
##'for (i in seq_along(pZ)) mkEllipse(do.call(cbind, contourLines(tripBridge:::as.image.pimg.list(pZ[i]), quantile(x$z, 0.95))[[1]][c("x", "y")]))


#'  #Example to generate contours and ellipsoidhulls and plot partitioned in time
#' library(cluster)
#'
#' cllist  <- lapply(pZ, function(x) {x <- tripBridge:::as.local.pimg(x) ; contourLines(x, levels = quantile(x$z, 0.95))[[1]]})
#'
#' exylist <- lapply(cllist, function(x) ellipsoidhull(cbind(x$x, x$y)))
#'
#'
#' ct <- cut(attr(pZ, "times"), "10 days")
#'
#' cl <- vector('list', nlevels(ct))
#' zl <- cl
#' el <- cl
#' for (i in seq_along(cl)) {
#'   zl[[i]] <-  as.image.pimg.list(pZ, which(ct == levels(ct)[i]))
#'   ##print( which(ct == levels(ct)[i]))
#'   ## drop the edges, need trim.pimg for this
#'   goodx <- rowSums(zl[[i]]$z) > 0
#'   goody <- colSums(zl[[i]]$z) > 0
#'   zl[[i]]$z <- zl[[i]]$z[goodx, goody]
#'   zl[[i]]$x <- zl[[i]]$x[goodx]
#'   zl[[i]]$y <- zl[[i]]$y[goody]
#'   ##image(zl[[i]], add = TRUE, col = rainbow(256));scan()
#'   cl[[i]] <- contourLines(zl[[i]], levels = quantile(zl[[i]]$z[zl[[i]]$z > 0], 0.5))[[1]]
#'   el[[i]] <- ellipsoidhull(cbind(cl[[i]]$x, cl[[i]]$y))
#' }
#'
#'
#' image(as.image.pimg.list(pZ), col = c("transparent", rainbow(2560)))
#' i <- 0
#' i <- i + 1;lines(cl[[i]]); lines(predict(el[[i]]), lwd = 2)
#'
#' gc1 <- gcIntermediate(coordinates(tr)[1, ], coordinates(tr)[158,])
#' lines(gc1)
#'
#' gc2 <- gcIntermediate(coordinates(tr)[158, ], coordinates(tr)[nrow(tr),])
#' lines(gc2)





#'## animate
#'require(animation)
#'require(maptools)
#'data(wrld_simpl)
#'wrld <- wrld_simpl[grep("Australia", wrld_simpl$NAME), ]
#'oopt = ani.options(nmax = 20, ani.width = 600, ani.height = 500, interval = 0.2)
#'ani.start()
#'for (i in seq_along(pZ1)) {
#'   plot(pZ[[1]]$xbound[1:2], pZ[[1]]$ybound[1:2], type = "n")
#'  plot(wrld, add = TRUE, col = "grey")
#'  image(as.image.pimg.list(pZ[i]), col = c("transparent", rainbow(256)), add = TRUE)
#'   tailindex <- (1:10) + (i - 15)
#'   tailindex <- tailindex[tailindex > 0 & tailindex < length(pZ)]
#'   if (length(tailindex) > 5) contour(as.image.pimg.list(pZ[tailindex]), add = TRUE)
#'}
#'ani.stop()
#'ani.options(oopt)

#'
#'## summarize as if we have run a threshold.mcmc model
#'load("~/Documents/GIT/tripEstimation_datacache/SBW_/SBW_Sensitivity_20Krun.Rdata")
#'bb <- rbind(range(c(fit$set[,1], fit$rise[,1])), range(c(fit$set[,2], fit$rise[,2])))
#'pX <- pimg.list(times = c(set, rise), xlim = bb[1,], ylim = bb[2,], img.dim = c(150, 120), Z = FALSE)
#'a <- array(0, c(2, ncol(fit$rise), nrow(fit$rise)))
#'a[1,,] <- t(fit$set)
#'a[2,,] <- t(fit$rise)
#'
#'pX <- behav.bin(a, pX)
#'
#'op <- par(mfrow = c(2, 1))
#'image(as.image.pimg.list(pX[1]), add = FALSE, col = c("transparent", trip::oc.colors(256)))
#'image(as.image.pimg.list(pX[2]), add = FALSE, col = c("transparent", trip::oc.colors(256)))
#'par(op)
#'}
#'@export
#'@keywords manip
`pimg.list` <-
  function(times, xlim, ylim, img.dim, Z = TRUE) {

    ## sanity checks on times, xlim, ylim
    if (any(diff(unclass(times)) <= 0 )) stop("times must be in increasing order")
    stopifnot(length(xlim) == 2L)
    stopifnot(length(ylim) == 2L)
    stopifnot(length(img.dim) == 2L)
    stopifnot(length(times) >= 2L)

    stopifnot(diff(xlim) > 0)
    stopifnot(diff(ylim) > 0)
    stopifnot(all(img.dim > 1))

    #epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
    #tm <- 12*3600*(round(unclass(times)/(12*3600))) + epoch
    tm <- times

    ## if these are Zs, then the times specify the X times
    n <- length(tm) - Z

    lst <- vector("list",length=n)
    for(i in 1:n) lst[[i]] <- pimg(xlim[1], xlim[2], img.dim[1], ylim[1], ylim[2], img.dim[2])

    if (Z) {
      Ztimes <- tm[-length(tm)] + diff(unclass(tm))/2
      attr(lst, "times") <- Ztimes
      attr(lst, "Xtimes") <- tm
    } else {
      attr(lst, "times") <- tm
    }
    attr(lst, "Z") <- Z
    attr(lst, "itersbin") <- 0
    class(lst) <- c("pimg.list", "list")
    lst
  }

##' Interleave lists of Probability images
##'
##' Interleave
##' @title interleave pimg.list
##' @param px pimg.list
##' @return pimg.list
##' @author Michael D. Sumner
`interleave.pimg.list` <-
function (px)
{
    epoch <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")
    tms <- unlist(lapply(px, function(x) attr(x, "times")))
    Zs <- unlist(lapply(px, function(x) attr(x, "Z")))
    if (sum(Zs) > 0 && !all(Zs))
        stop("mixture of X and Z pimg.lists not supported")
    tms <- 12 * 3600 * (round(unclass(tms)/(12 * 3600))) + epoch
    xb <- px[[1]]$xbound
    yb <- px[[1]]$ybound
    p <- pimg.list(tms, xlim = c(xb[1], xb[2]), ylim = c(yb[1],
        yb[2]), img.dim = c(xb[3], yb[3]), Z = all(Zs))
    l <- list()
    for (ix in px) l <- c(l, ix)
    for (ip in 1:length(l)) p[[ip]] <- l[[ip]]
    names(p) <- names(tms)
    p
}



##' ##' Converts Probability image list (pimg.list) to a Global R (xyz) list image.
##'
##'
##' @title Convert to image list
##' @param pimgs List of probability images (pimg.list)
##' @param subset optional sub-index of elements to include from the list
##' @return R xyz image list
##' @author Michael D. Sumner
##' @keywords manip
##' @export
`as.image.pimg.list` <-
    function (pimgs, subset = seq_along(pimgs))
{
    ## should have checks elsewhere for these NULLs, do they persist when no mixing?
    bad <- unlist(lapply(pimgs, function(x) is.null(x$image)))[subset]
    subset <- subset[!bad]
    res <- as.image.pimg(pimgs[[subset[1]]])
    if (length(subset) == 1)
        return(res)
    for (i in subset[-1]) {
        img <- pimgs[[i]]
        Xpos <- img$offset[1]
        Ypos <- img$offset[2]
        Xind <- Xpos:(Xpos + dim(img$image)[1] - 1)
        Yind <- Ypos:(Ypos + dim(img$image)[2] - 1)
        res$z[Xind, Yind] <- res$z[Xind, Yind] + img$image
    }
    res
}


#'Convert to Global image
#'
#'Converts Probability image (pimg) to a Global R (xyz) list image.
#'
#'
#'@aliases as.image.pimg
#'@usage as.image.pimg(pimg)
#'@param pimg Probability image
#'@author Michael D. Sumner
#'@keywords manip
`as.image.pimg` <-
function(pimg) {
  img <- coords.pimg(pimg)
  img$z <- as.matrix.pimg(pimg)
  img
}

#'Convert to a Global R matrix
#'
#'Convert Probability image (pimg) to a Global R matrix
#'
#'
#'@aliases as.matrix.pimg
#'@usage as.matrix.pimg(x)
#'@param x Probability image
#'@author Michael D. Sumner
#'@keywords manip
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


#'Extract coordinates from pimg object
#'
#'Converts Probability image (pimg) to the Global coordinates of an R (xyz) list image.
#'
#'@aliases coords.pimg
#'@usage coords.pimg(pimg)
#'@param pimg Probability image
#'@author Michael D. Sumner
#'@keywords manip
`coords.pimg` <-
  function(pimg) {
    list(x=seq(pimg$xbound[1],pimg$xbound[2],length=pimg$xbound[3]),
         y=seq(pimg$ybound[1],pimg$ybound[2],length=pimg$ybound[3]))
  }

#'Convert to Local image
#'
#'Converts Probability image (pimg) to a Local R (xyz) list image.
#'
#'
#'@aliases as.local.pimg
#'@usage as.local.pimg(pimg)
#'@param pimg Probability image
#'@author Michael D. Sumner
#'@keywords manip
"as.local.pimg" <-
function(pimg) {
  img <- coords.pimg(pimg)
  img$x <- img$x[pimg$offset[1]:(pimg$offset[1] + nrow(pimg$image)-1)]
  img$y <- img$y[pimg$offset[2]:(pimg$offset[2] + ncol(pimg$image)-1)]
  img$z <- pimg$image
  img
}

#'Bin MCMC chains.
#'
#'Bin MCMC chains to list of Probability images (pimg.list).
#'
#'
#'@aliases behav.bin
#'@usage behav.bin(z, pimgs, weights = NULL)
#'@param z \code{z}
#'@param pimgs \code{pimgs}
#'@param weights \code{weights}
#'@export
#'@keywords manip
#'
`behav.bin` <-
function (z, pimgs, weights = NULL)
{
    if (is.null(weights) && attr(pimgs, "Z"))
        weights <- c(diff(unclass(attr(pimgs, "Xtimes"))/3600))
    if (is.null(weights)) weights <- rep(1, length(pimgs))


    if (!(length(weights) == length(pimgs))) stop("length of weights do not match length of p-img list")
    if (nrow(z) != length(pimgs))
        stop("dimensions of chain do not match length of p-img list")
#dm <- dim(z)


    for (k in seq_along(weights)) {
        pimgs[[k]] <- bin.pimg(pimgs[[k]], t(z[k, 1:2, ]), weight = weights[k])
    }
    ## should also have a flag for whether this is initialized/scaled, so iter number is independent
    attr(pimgs, "itersbin") <- attr(pimgs, "itersbin") + dim(z)[3]
    pimgs
}

##' Rescale Probability image list (pimg.list) for iterations
##'
##' Divide out the number of iterations stored in a pimg.list.
##' @title rescale pimg.list
##' @param x pimg.list
##' @return pimg.list
##' @author Michael D. Sumner
`scale.pimg.list` <-
  function(x) {
    for (i in seq_along(x)) {

        x[[i]]$image <- x[[i]]$image / attr(x, "itersbin")
    }
    ##attr(x, "itersbin") <- 0
    x
  }


#'Bin MCMC chain.
#'
#'Bin MCMC chain to Probability image (pimg).
#'
#'
#'@aliases bin.pimg
#'@usage bin.pimg(pimg, xy, weight = 1)
#'@param pimg Probability image
#'@param xy matrix of coordinates to bin
#'@param weight weight to apply to this bin
#'@keywords manip
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

#'Create a probability image, for MCMC binning.
#'
#'Pimage.
#'
#'
#'@aliases pimg
#'@usage pimg(xmin, xmax, xn, ymin, ymax, yn)
#'@param xmin minimum x-coordinate
#'@param xmax maximum x-coordinate
#'@param xn number of cells for x-dimension
#'@param ymin minimum y-coordinate
#'@param ymax maximum y-coordinate
#'@param yn number of cells for y-dimension
`pimg` <-
function(xmin,xmax,xn,
                 ymin,ymax,yn) {
  res <- list(xbound=c(xmin,xmax,xn),
       ybound=c(ymin,ymax,yn),
       offset=c(1,1),
       image=NULL)

       class(res) <- c("pimg", "list")
  res
}



