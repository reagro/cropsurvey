
sampler <- function(crp, adm, ttime, ssize=100, th=100, n=4) {
	out <- list()
	ss <- ssize/(2^n)
	for (i in 1:nlyr(crp)) {
		r <- crp[[i]]
		csel <- ifel(r > th, r, NA)
		r <- divider(r, n, border=adm, rasterize="yes")
		if (!is.null(ttime)) {
			r <- mask(r, ttime, inverse=TRUE, maskvalue=FALSE)
		}
		out[[i]] <- spatSample(r, ss, method="stratified", as.points=TRUE, weights=csel)
	}
	v <- crds(vect(out))
	vect(unique(v), crs=crs(crp))
}  


get_sample <- function(x, adm, ttime=NULL, dmin=30000, ssize=100, th=100, splits=4, seed=999) {
	set.seed(seed)
	if (!is.null(ttime)) {
		qtime <- global(ttime, \(i) quantile(i, 0.8, na.rm=T))[[1]]
		ttime <- ttime > qtime
	}
	s <- sampler(x, adm, ttime, ssize=ssize, th=th, n=splits) 
	u <- collapse_dist(s, dmin)
	nr <- nrow(u)
	for (i in 1:10) {
		u <- collapse_dist(u, dmin)
		if (nrow(u) == nr) break
		nr <- nrow(u)
	}
	values(u) <- extract(x, u)
	u
}


