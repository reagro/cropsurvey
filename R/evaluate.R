# Author: Robert Hijmans
# January 2022
# License GPL3

evaluate_sample <- function(x, s, main=names(x), plot=FALSE) {
	e <- extract(x, s)[,-1, drop=FALSE] |> na.omit()
	x <- na.omit(values(x))
	main <- rep_len(main, ncol(x))
	for (i in 1:ncol(x)) {
		xx <- x[,i]
		xx <- xx[xx > 0]
		ee <- e[,i]
		ee <- ee[ee > 0] |> sort()
		reg <- sapply(1:10, \(i) sample(xx, length(ee), prob=xx) |> sort()) |> rowMeans()
		#dim(#reg <- sample_regular(sort(xx), length(ee))
		w <- stats::wilcox.test(reg, ee)
		if (plot) {
			p <- round(w$p.value, 3)
			r <- c(0, max(c(reg, ee)))
			plot(reg, ee, main=paste0(main[i], "  (", p, ")"), las=1,
				xlab="distribution", ylab="sample", xlim=r, ylim=r)
			graphics::abline(0,1)
		} 
	}
	invisible(w$p.value |> round(4) |> formatC(digits=4, format="f"))
}

