# Author: Robert Hijmans
# January 2022
# License GPL3

splitNS <- function(x) {
	v <- aggregate(x, c(1, ncol(x)), sum, na.rm=T) |> values() |> cumsum()
	m <- which.min(abs(v - max(v)/2))
	list(n=x[1:m, ,drop=FALSE], s=x[(m+1):nrow(x), ,drop=FALSE])
}

splitWE <- function(x) {
	v <- aggregate(x, c(nrow(x), 1), sum, na.rm=T) |> values() |> cumsum()
	m <- which.min(abs(v - max(v)/2))
	list(w=x[, 1:m, drop=FALSE], e=x[, (m+1):ncol(x), drop=FALSE])
}

divider <- function(x, n=2, start="ns", border=NULL, rasterize="no") {

	rasterize <- match.arg(tolower(rasterize), c("yes", "no", "both"))
	if (!is.null(border)) stopifnot(inherits(border, "SpatVector"))
	n <- max(round(n), 1)
	start <- match.arg(tolower(start), c("ns", "ew"))
	north <- start == "ns"
	
	x <- classify(x, cbind(NA, 0))
	out <- list(x)
	for (i in 1:n) {
		if (north) {
			out <- unlist(lapply(out, \(i) splitNS(i)))
			north <- FALSE
		} else {
			out <- unlist(lapply(out, \(i) splitWE(i)))
			north <- TRUE
		}
	}  
	out <- lapply(out, \(i)as.polygons(ext(i))) |> vect()
	crs(out) <- crs(out)
	out$zones <- 1:nrow(out)
	if (!is.null(border)) {
		out <- crop(out, border)
	}
	if (rasterize != "no") {
		r <- rasterize(out, x, "zones")
		if (rasterize == "both") {
			return(list(r=r, v=out))
		} else {
			return(r)
		}
	} else {
		out
	}
}

