
## code to become a "reflection" function to plot light data

## does not work so well on the snow petrel, test again on toolshed and good seal datasets


start <- c(62.82567, -67.58955)

##load("C:\\Users\\michae_sum\\Documents\\data\\snowpetrel\\plotanimation\\snowpetrelrawdataSegment.Rdata")

d <- read.table("C:\\Users\\michae_sum\\Documents\\data\\snowpetrel\\plotanimation\\18A042_000.lig", sep = ",")
names(d) <- c("ok", "datetime", "exceltime", "light")

d$gmt <- as.POSIXct(strptime(d$datetime, "%d/%m/%y %H:%M:%S"), tz = "GMT")

library(tripEstimation)

d$local <- d$gmt +  (start[1] / 15) * 3600


## solar elevation based on site location
sun <- solar(d$gmt)
d$el <- elevation(start[1], start[2], sun)

d$day <- format(d$local, "%j")
uday <- unique(d$day)



ylim <- range(d$light) + c(-1, 1) * 5
par(mfrow = c(3, 1))
for (i in 1:length(uday)) {

    subsub <- d$day == uday[i]
    local <- d$local[subsub]
    xtk <- trunc(local[1], "day") + seq(0, 1, length = 7) * 24 * 3600

    light <- d$light[subsub]
    gmt <- d$gmt[subsub]
    el <- d$el[subsub]

    e.val <- local[which.max(el)]
 dtime <- unclass(local) - unclass(as.POSIXct(trunc(local[1], "day")))
 offset <- e.val + 12 * 3600
    reflecttime <- offset - dtime

    plot(local, light, col = "blue", lwd = 4, type = "l",
               xlim = range(xtk), axes = FALSE,
               ylim = ylim, xlab = format(local[1], "%Y-%m-%d"))
lines(reflecttime, light)

    par(new = TRUE)
            plot(local, el, type = "l",
                 xlim = range(xtk),
                 ylim = c(-30, 30),
               axes = FALSE, xlab = "", ylab = "")
axis(1, at = xtk, lab = format(xtk, "%H:%M:%S"))
axis(4)

    if (i %% 3 == 0) scan("", 1)
}
