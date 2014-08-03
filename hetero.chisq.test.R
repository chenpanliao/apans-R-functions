

hetero.chisq.test <- function (
	mat,
	p = rep (1 / ncol (mat), ncol (mat))
) {

	# check integer
	if (!all (abs (mat - round (mat)) < .Machine$double.eps^0.5)) {
		stop ("All atoms in the input matrix must be integers.")
	}
	
	# check dimension
	if (nrow (mat) < 2 | ncol (mat) < 2) {
		stop ("The number of row and that of columes must be greater than 1")
	}
	
	# chi-square of rows
	rows.tests <- apply (mat, 1, chisq.test, correct = F, p = p)
	rows.chisq <- sum (sapply (rows.tests, function (.) .$statistic))

	# chi-square of pooled
	pool.chisq <- chisq.test (colSums (mat), correct = F, p = p)$statistic
	attr(pool.chisq, "names") <- NULL
	pool.df <- ncol (mat) - 1
	
	# chi-square test of heterogenity
	hetero.chisq <- rows.chisq - pool.chisq
	hetero.df <- (ncol (mat) - 1) * nrow (mat) - pool.df
	hetero.p <- pchisq (hetero.chisq, hetero.df, lower.tail = F)
	
	ret <- list (
		mat = mat, 
		category.n = ncol (mat),
		category.names = colnames (mat),
		replication.n = nrow (mat),
		expect.p = p,
		statistic = hetero.chisq,
		df = hetero.df,
		chisq.p = hetero.p
	)
	class (ret) <- "hetero.chisq.test"
	return (ret)
	
}

print.hetero.chisq.test <- function (x) {

	r1 <- rbind (
		`Number of category` = x$category.n,
		`Number of replication` = x$replication.n,
		`Heterogenity Chi-square` = x$statistic,
		`Heterogenity DF` = x$df,
		`Heterogenity P` = x$chisq.p,
		colnames = NULL
	)
	colnames (r1) <- "Value"
	print (r1)

	r2 <- rbind (
		`Expected probability` = x$expect.p
	)
	colnames (r2) <- x$category.names
	print (r2)

}

plot.hetero.chisq.test <- function (x) {
	barplot (
#		t (x$mat),
		t (x$mat / apply(x$mat, 1, sum)) * 100,
		legend.text = x$category.names,
		names.arg = rownames (x$mat),
		ylab = "Percentage",
		xlab = "Replication"
	)
}







x <- scan ()
25 32 14 70 24 20 32 44 50 44
11 7 5 27 13 6 13 9 14 18

mat <- matrix (x, 10)
colnames (mat) <- c("Yellow.Seeds", "Green.Seeds")
rownames (mat) <- 1:10
m <- hetero.chisq.test (mat, c(0.75,0.25))
m
plot(m)
