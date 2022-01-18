# Author: Robert Hijmans
# January 2022
# License GPL3

sample_regular <- function(x, n, sorted=FALSE) {
	nx <- length(x)
	if (n > nx) stop("n is larger than the length of x")
	if (n == nx) return (x)
	s <- nx / (n + 1)
	i <- seq(s, nx, s)[1:n]
	if (sorted) {
		sort(x)[i]
	} else {
		x[i]
	}
}

