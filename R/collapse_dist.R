
collapse_dist <- function(v, threshold) {
	xy <- crds(v)
	lonlat <- is.lonlat(v)
	out <- vector(mode = "list", length = nrow(xy))
	i <- 1
	while(nrow(xy) > 1) {
		d <- distance(xy[1,,drop=FALSE], xy[-1,,drop=FALSE], lonlat=lonlat) 
		if (min(d) < threshold) {
			w <- which.min(d)
			out[[i]] <- cbind(mean(c(xy[1,1], xy[w,1])), mean(c(xy[1,2], xy[w,2])))
			xy <- xy[-c(1,w), , drop=TRUE]
		} else {
			out[[i]] <- xy[1,]
			xy <- xy[-1, , drop=FALSE]
		}
		i <- i + 1
	}
	out[[i]] <- xy
	out <- do.call(rbind, out)
	vect(out, crs=crs(v))
}

