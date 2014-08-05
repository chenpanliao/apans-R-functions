# Copyright 2013 Chen-Pan Liao 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#		 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#		 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# See http://apansharing.blogspot.tw/2013/06/g-test-for-2-way-contingency-data-in-r.html
# to learn how to use this function

g.test.2way <- function(
	x # must be a 2-way xtabs/matrix
){
	## initiating
	data.o <- x
	sum.total <- sum(x)
	sum.row <- rowSums(x)
	sum.col <- colSums(x)
	n.row <- dim(x)[1]
	n.col <- dim(x)[2]
	chisq.df <- (n.col - 1) * (n.row - 1)
	
	## check
	if(n.row == 1) {stop("Numbers of row must be larger than 1")}
	if(n.col == 1) {stop("Numbers of column must be larger than 1")}
	if(mode(data.o) != "numeric") {stop("Mode of x must be numeric")}
	if(
		sum(class(data.o) != c("matrix", "xtabs")) == 0
	) {
		stop("Class of x must be matrix or xtabs")
	}
	
	## matrix of expected value
	data.e <- matrix(NA, ncol = n.col, nrow = n.row)
	for (m in 1:n.row){
		for (n in 1:n.col){
			data.e[m,n] <- sum.col[n] * sum.row[m] / sum.total
		}
	}
	
	## matrix of ovserved value with Yates' correction
	data.o.y <- matrix(NA, ncol = n.col, nrow = n.row)
	for (m in 1:n.row){
		for (n in 1:n.col){
			if (data.e[m,n] - data.o[m,n] > 0){
				data.o.y[m,n] <- data.o[m,n] + 0.5
			} else if (data.e[m,n] - data.o[m,n] < 0){
				data.o.y[m,n] <- data.o[m,n] - 0.5
			} else {
				data.o.y[m,n] <- data.o[m,n]
			}
		}
	}

	## q_min for Williams' correction
	q.min <- 1 + 
					 (sum.total * sum(1 / sum.col) - 1) *
					 (sum.total * sum(1 / sum.row) - 1) /
					 (6 * (chisq.df) * sum.total)


	## G-value
	isnot.na.index <- !is.na(data.o * log(data.o / data.e))
	g <- 2 * sum((data.o * log(data.o / data.e))[isnot.na.index])
	isnot.na.index.y <- !is.na(data.o.y * log(data.o.y / data.e))
	g.y <- 2 * sum((data.o.y * log(data.o.y / data.e))[isnot.na.index])
	g.w <- g / q.min
	
	## P-value
	p.g <- 1 - pchisq(g, chisq.df)
	p.g.y <- 1 - pchisq(g.y, chisq.df)
	p.g.w <- 1 - pchisq(g.w, chisq.df)


	## return
	z <- list(
		data.observed = data.o,
		data.expected = data.e,
		data.observed.Yates = data.o.y,
		df = chisq.df,
		q.min = q.min,
		g = g,
		g.Yates = g.y,
		g.Williams = g.w,
		p = p.g,
		p.Yates = p.g.y,
		p.Williams = p.g.w
	)
	class(z) <- "g.test.2way"
	return(z)
}

## print function
print.g.test.2way <- function(x){
	cat("Observed value:\n")
	print(x$data.observed)
	cat("Expected value:\n")
	print(x$data.expected)
	cat("Observed value with Yates' correction:\n")
	print(x$data.observed.Yates)
	cat("Degree of freedom =", x$df, "\n")
	cat(sprintf("G-test:\n\tG = %g,\tp = %g\n", x$g, x$p))
	cat(sprintf(
		"G-test with Yates' correction:\n\tG = %g,\tp = %g\n",
		x$g.Yates, x$p.Yates
	))
	cat(sprintf(
		"G-test with Williams' correction:\n\tG = %g,\tq_min = %g,\tp = %g\n",
		x$g.Williams, x$q.min, x$p.Williams
	))
}