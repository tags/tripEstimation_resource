## utility functions to extract raw coordinates from all (potentially
## heirarchical) line and polygon objects in sp package

## library(maptools)
## data(wrld_simpl)
## xy <- pCoords(wrld_simpl)
## xy <- lCoords(as(wrld_simpl, "SpatialLinesDataFrame"))

lCoords <- function(p) do.call("rbind", lapply(p@lines, function(x1)
do.call("rbind", lapply(x1@Lines, function(x2)
x2@coords[-nrow(x2@coords), ]))))



pCoords <- function(p) do.call("rbind", lapply(p@polygons, function(x1)
do.call("rbind", lapply(x1@Polygons, function(x2)
x2@coords[-nrow(x2@coords), ]))))


