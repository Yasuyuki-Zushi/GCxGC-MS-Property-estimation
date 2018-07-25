gcxgc_contour <- function (xx1) {
	t1.vector.append <- 0
	t2.vector.append <- 0
	for (xx1.i in 1:length(xx1) ){
		t1.vector <- rep(xx1[xx1.i],row)
		t2.vector <- yy1
		t1.vector.append <- c(t1.vector.append, t1.vector)
		t2.vector.append <- c(t2.vector.append, t2.vector)
	}
	t1.vector.append <- t1.vector.append[-c(1)]
	t2.vector.append <- t2.vector.append[-c(1)]

	t1 <<- t1.vector.append 
	t2 <<- t2.vector.append
	cat("t1, t2 are calculated.")
	writeLines(sep="\n","")
}
