as.POSIXct.trip <- function(x) {res <- x[[getTORnames(x)[1]]];names(res) <- x[[getTORnames(x)[2]]];res}


bin.trip <- function(x, cutstring = NULL) {
	  tms <- as.POSIXct(x)
	ct <- cut(tms, "1 sec")

	xx <- as.POSIXct(levels(ct))
	xx <- c(xx, max(tms))

	x.list <- cut(x, xx)
	g <- makeGridTopology(x)
	pim <- Pimage(xx, grid = g, Z = TRUE)

	
	for (i in seq_along(x.list)) {
		tg <- tripGrid(x.list[[i]], grid = g)
		
	}	

	
}
