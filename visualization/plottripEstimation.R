
plotthestuff <- function(xlim, ylim, proj = "+proj=laea") {
    require(maptools)
    require(rgdal)
    require(raster)
    require(rgeos)
    xcentre <- round(xlim[1] + diff(xlim)/2)
    ycentre <- round(ylim[1] + diff(ylim)/2)
    if (length(grep("+lon_0", proj)) == 0) {
        proj <- sprintf("%s +lon_0=%f +lat_0=%f", proj, xcentre, ycentre)
    }

    data(wrld_simpl)
    buf <- 0
    bufx <- diff(xlim) * buf
    bufy <- diff(ylim) * buf
    wrld_simpl <- gIntersection(wrld_simpl, as(extent(xlim[1] - bufx, xlim[2] + bufx, ylim[1] - bufy, ylim[2] + bufy), "SpatialPolygons"))
    mp <- spTransform(wrld_simpl, CRS(proj))

    gl <- gridlines(wrld_simpl)
    gl <- spTransform(gl, CRS(proj))
     ga <- gridat(wrld_simpl, side="WS", offset = 0)
     ga <- spTransform(ga, CRS(proj))
    ## bug in gridat should use stringsAsFactors = FALSE
    ga$labels <- as.character(ga$labels)




    function(chain, legend = TRUE, returnproj = FALSE) {
        if (returnproj) return(proj)
            xm <- apply(chain$x, 1:2, mean)
            pxm <-    project(xm, proj)
            plot(pxm, type = "n", asp = 1, xlab = "", ylab = "", axes = FALSE)
            plot(mp, col = "transparent", asp = 1,  xlab = "", ylab = "", axes = FALSE)
            ##box()
            plot(mp, add = TRUE, col ="lightgrey")
            lines(project(chain$x[,,1], proj), lwd = 2)
            lines(project(chain$last.x, proj),col="red")
op <- par(xpd = TRUE)

            plot(spTransform(gl, CRS(proj)), col = "darkgrey", lty = 2, add = TRUE)
  text(coordinates(ga), labels=parse(text=ga$labels), pos=ga$pos,
       offset=ga$offset)

            usr <- par("usr")
            legend(usr[1], usr[4],
                   legend = c("first sample", "last.sample"),
                   lwd = c(2, 1),
                   col = c("black", "red"),
                   lty = 1, bg = rgb(1, 1, 1, 0.7))

par(op)

    }
}




