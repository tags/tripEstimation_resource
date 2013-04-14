##https://sites.google.com/site/slommihomepage/r-1/world-map/orthographic-projection

###FUNCTIONS

rad <- function(x){x/180*pi}
deg <- function(x){x*180/pi}

###Funktio cross
cross <- function(koe) {    #inputtina "koe"
cross <- 0 # up,down,up etc. -1=down
for(i in 1:(nrow(koe)-1)) { #ei huomioi ensimm. ja viimeisen pisteen suhdetta
if(koe[i,3] >= 0 & koe[i+1,3] < 0) cross <- c(cross, -1*(i+1)) #first neg
if(koe[i,3] < 0 & koe[i+1,3] >= 0) cross <- c(cross, i) #last neg
}
cross <- cross[-1]
return(cross)
}

###Funktio frameshift
frameshift <- function(koe, cr) {    #inputtina "koe" ja cross(koe)
shift <- abs(max(cr[cr < 0]))-1 #one before first down-node
koe <- rbind(koe[shift:(nrow(koe)-1),], koe[1:shift,])
#koe <- rbind(koe[shift:nrow(koe),], koe[1:(shift-1),])
}


#testaa frameshift-funktiota graafisesti:
#plot(koe[,3], type="l")#see how much of polygons is "negative"
#abline(h=0)
#koe2 <- frameshift(koe, cross(koe))
#lines(koe2[,3], col="blue")

###Funktio ortho
ortho <- function(crds, lat_1=0, lon_0=0) { #crds in in deg
lat_1 <- rad(lat_1)
lon_0 <- rad(lon_0)
crds <- cbind(rad(crds[,1]), rad(crds[,2]), 0, 0, crds[,1], crds[,2], crds[,3]) # add columns between
for(i in 1:nrow(crds)) {
lat <- crds[i,2]
lon <- crds[i,1]
crds[i,1] <- cos(lat)*sin(lon-lon_0)
crds[i,2] <- cos(lat_1)*sin(lat)-sin(lat_1)*cos(lat)*cos(lon-lon_0)
crds[i,3] <- sin(lat_1)*sin(lat)+cos(lat_1)*cos(lat)*cos(lon-lon_0) #append check column, horizon distance
}
colnames(crds) <- c("rlon", "rlat", "horiz", "vis", "dlon", "dlat", "ID")
return(crds)
}


### here you define the center point of the projection, for poles use
### 89.99 instead of 90

lat1 <- 0
lon0 <- 0

library(maptools)
data(wrld_simpl)
oz <- wrld_simpl[wrld_simpl$NAME == "Australia", ]
xy <- cbind(coordinates(as(oz, "SpatialLinesDataFrame"))[[1]][[1]], 0)

xy <- cbind(pCoords(wrld_simpl), 0)

## a 7 column matrix  "rlon"  "rlat"  "horiz" "vis"   "dlon"  "dlat"  "ID"
## rlon/rlat is projected, dlon/dlat is original lonlat, horiz is values for visibility, vis is all 0
## pg.ortho is a series of polygon rings
pg.ortho <- ortho(xy, lat1, lon0)
## koe is a subset of one ring from pg.ortho, currently one and the same
j <- 0
koe <- which(pg.ortho[,"ID"]==j)

## remember this is done per polygon ring, so not sensible here
## visible 1 fully visible or 2 partly visible, or 0 none visible
if(max(na.omit(pg.ortho[koe, "horiz"])) > 0) pg.ortho[koe, "vis"] <- 1 # visible = 1
if(max(na.omit(pg.ortho[koe, "horiz"])) > 0 & min(na.omit(pg.ortho[koe, "horiz"])) < 0) pg.ortho[koe, "vis"] <- 2

#partially invisible pgons
mix <- unique(pg.ortho[which(pg.ortho[,"vis"]==2), "ID"])
