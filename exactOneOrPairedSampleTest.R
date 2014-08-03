# Copyright 2014 Chen-Pan Liao 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

# To understand more details about this function, see:
# Bryan F. J. Manly. 1997.
# Randomization, bootstrap and Monte Carlo methods 
# in biology. 2nd edition. pp. 91-97

## main function
exactOneOrPairedSampleTest <- function(
	x1,
	x2 = NULL,
	mu = 0,
	alternative = c("t","g","l")

	# x1		A numeric vector; necessary
	# x2		A numeric vector; necessary for paired test
	# mu		A numeric value;
	#		for one sample test: H0: x1 = mu;
	#		for pired sample test: H0: x1 - x2 = mu.
	# alternative	A string to define the rejected area;
	#		"t" for H1: abs(difference) > abs(mu)
	#		"g" for H1: difference > mu
	#		"l" for H1: difference < mu
	#		where difference is x1 for one sample test 
	#		or x1-x2 for paired sample

) {

	# determing one sample or paired sampe
	# var is.onesample
	if (is.null(x2)) {
		is.onesample <- T
	} else {
		is.onesample <- F
	}

	# initiation 
	# var DF, N
	x1.name <- deparse(substitute(x1))
	x2.name <- deparse(substitute(x2))
	alternative <- alternative[1]
	mu <- as.numeric(mu)
	x1 <- as.numeric(x1)
	if(is.onesample) {
		DF <- data.frame (x1, mu = mu)
	} else {
		x2 <- as.numeric(x2)
		DF <- data.frame (x1, x2)
	}
	DF <- na.omit(DF)
	n <- length(DF[,1])
	N <- 2^n

	# test statistic
	# var diff, test
	if (is.onesample) {
		DF$diff <- DF$x1 - mu
	} else {
		DF$diff <- DF$x1 - DF$x2 - mu
	}
	test.0 = mean(DF$diff)
	
	# binary matrix
	# var bmat
	bmat <- as.matrix(expand.grid(rep( list(c(1,-1)) , n)))
	
	# exact permutation
	test.perm <- numeric(N)
	for (i in 1:N) {
		diff.perm.sum <- DF$diff * bmat[i,]
		test.perm[i] <- mean(diff.perm.sum)
	}
	
	# p-value
	if (alternative == "t") {
		rejected.N <- sum( abs(test.perm) >= abs(test.0) )
	}
	if (alternative == "g") {
		rejected.N <- sum(test.perm >= test.0)
	}
	if (alternative == "l") {
		rejected.N <- sum(test.perm <= test.0)
	}
	p.value <- rejected.N / N
	
	# return
	out <- list(
		x1 = x1,
		x2 = x2,
		x1.name = x1.name,
		x2.name = x2.name,
		n = n,
		mu = mu,
		test.0 = test.0,
		is.onesample = is.onesample,
		alternative = alternative,
		test.perm = test.perm,
		DF = DF,
		N = N,
		p.value = p.value,
		rejected.N = rejected.N
	)
	class(out) <- "exactOneOrPairedSampleTest"
	return (out);
	
}

## print function
print.exactOneOrPairedSampleTest <- function(w){
	if (w$is.onesample){
		cat("\n\tExact one sample test\n\n")
	} else {
		cat("\tExact paired sample test\n\n")
	}
	cat("Alternative hypothesis: ")
	if (w$is.onesample){
		if (w$alternative == "t") {
			cat("mean of", w$x1.name, "is not equal to", w$mu, "\n")
		}
		if (w$alternative == "g") {
			cat("mean of", w$x1.name, "is greater than", w$mu)
		}
		if (w$alternative == "l") {
			cat("mean of", w$x1.name, "is less than", w$mu)
		}
	} else {
		if (w$alternative == "t") {
			cat("mean of", w$x1.name, "-", w$x2.name, "is not equal to", w$mu)
		}
		if (w$alternative == "g") {
			cat("means of", w$x1.name, "-", w$x2.name, "is greater than", w$mu)
		}
		if (w$alternative == "l") {
			cat("means of", w$x1.name, "-", w$x2.name, "is less than", w$mu)
		}
	}
	cat("\n")
	if (w$is.onesample){
		cat(
			paste(
				"mean(", w$x1.name, ") - mu = ", w$test.0, "\n",
				sep=""
		))
	}
	if (!w$is.onesample){
		cat(
			paste(
				"mean((", w$x1.name, ")-(", w$x2.name, ")) - mu = ",
				w$test.0,
				"\n",
				sep=""
			)
		)
	}
	cat("Number of total permutation =", w$N, "\n")
	cat("Number of rejected permutation =", w$rejected.N, "\n")
	cat("P-value =", w$p.value, "\n")
}

## hist function
hist.exactOneOrPairedSampleTest <- function(w){
	if (w$is.onesample){
		hist(
			w$test.perm, breaks=60, 
			xlab=paste(
				"mean(", w$x1.name, ") - mu",
				 sep=""
			 ), main=""
		)
	} else {
		hist(
			w$test.perm, breaks=60,
			xlab=paste(
				"mean((", w$x1.name, ")-(", w$x2.name, ")) - mu",
				sep=""
			), main=""
		)
	}
	title("Histogram of exact permutation")
	abline(v=w$test.0, lty=2)
	if (w$alternative == "t") {
		abline(v=w$test.0*(-1), lty=2)
	}
}