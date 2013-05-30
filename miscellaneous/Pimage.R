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
    if (Z) {
        Ztimes <- tm[-length(tm)] + diff(unclass(tm))/2
        attr(pim, "times") <- Ztimes
        attr(pim, "Xtimes") <- tm
    }
    else {
        attr(pim, "times") <- tm
    }
    attr(pim, "Z") <- Z
    class(pim) <- c("Pimage")
    pim
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
    cat("Class    :", class(x), "\nLength    :", length(x),  "\nTemporal Extent :", trange, "\n")
    ##cat("Time Steps   :")
    ##str(attr(x, "times"))
    print(ext)
    invisible(NULL)
}
#summary.Pimage
is.Pimage <- function(x) {inherits(x, "Pimage")}

###str.Pimage
#"[.Pimage"
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
    ## TODO, patch in the data
    x <- x[[1]]
    raster(nrows = x$xbound[3L], ncols = x$ybound[3L], xmn = x$xbound[1L], xmx = x$xbound[2L], ymn = x$ybound[1L], ymx = x$ybound[2L])
}
