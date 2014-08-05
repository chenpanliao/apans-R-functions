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

# See http://apansharing.blogspot.tw/2013/12/an-r-function-for-bootstrap-confidence.html
# to learn how to use this function

## main function
bootCI <- function(
	x,
	alpha=0.05,
	alternative=c("t", "l", "g"),		# see arg "alternative" in help(t.test)
	B=1999,													# number of bootstrap samples
	quantileAlgorithm = 7						# passed to quantile(type)
){
	#validation
	if(mode(x) != "numeric"){
		stop("mode(x) must be \"numeric\"")
	}
	if(class(x) != "numeric" && class(x) != "integer"){
		stop("class(x) must be \"numeric\" or \"integer\"")
	}
	if(length(x) < 2){
		stop("length(x) must be larger than 1")
	}
	
	# initiation
	alternative <- alternative[1]
	stat.length <- length(x)
	stat.mean <- mean(x)
	stat.sem <- sqrt(var(x) / stat.length)
		
	# alternative
	probs <- c(alpha/2, 1-alpha/2)
	if(alternative == "l") probs <- c(0, 1-alpha)
	if(alternative == "g") probs <- c(alpha, 1)
 
	# bootstrap
	boot.mat <- matrix(0.0, B+1, stat.length)
	boot.mat[1,] <- x
	for (i in 2:(B+1)) {
		boot.mat[i,] <- sample(x, replace=T)
	}

	# exact (traditional) CI based on t distribution
	CI.exact <- stat.mean + qt(probs, stat.length - 1) * stat.sem
	names(CI.exact) <- paste(probs*100, "%", sep="")
	
	# basic bootstrap CI
	CI.basic <- 2 * stat.mean - quantile(
		apply(boot.mat, 1, mean), probs=1-probs, type=quantileAlgorithm
	)
	names(CI.basic) <- rev(names(CI.basic))

	# percentile bootstrap CI
	CI.percentile <- quantile(
		apply(boot.mat, 1, mean), probs=probs, type=quantileAlgorithm
	)

	# studentized bootstrap CI
	tmp.mean <- apply(boot.mat, 1, mean)
	tmp.sem <- apply(
		boot.mat, 1, function(.){sqrt(var(.) / length(.))}
	)
	tmp.t <- (tmp.mean - stat.mean) / tmp.sem
	expr <- expression(
		CI.studentized <- 
			stat.mean - quantile(
				tmp.t, probs=1-probs, type=quantileAlgorithm
			) * stat.sem
	)
	a.try <- try(eval(expr), T)
	if("try-error" %in% class(a.try)) {
		warning("studentized bootstrap CI cannot 
			be done well due to boostrap sem = 0")
	}
	names(CI.studentized) <- rev(names(CI.studentized))
	
	# exceptions of one-tail
	if(alternative == "g") {
		CI.exact[2] = Inf
		CI.basic[2] = Inf
		CI.percentile[2] = Inf
		CI.studentized[2] = Inf
	} else if(alternative == "l") {
		CI.exact[1] = -Inf
		CI.basic[1] = -Inf
		CI.percentile[1] = -Inf
		CI.studentized[1] = -Inf
	}
	
	output <- list(
		x = x,
		alpha = alpha,
		alternative = alternative,
		B = B,
		CI.exact = CI.exact,
		CI.basic = CI.basic, 
		CI.percentile = CI.percentile, 
		CI.studentized = CI.studentized
	)
	class(output) <- "bootCI"
	return(output)
}

## print function
print.bootCI <- function(w){
	cat("Summary of x\n")
	print(summary(w$x))
	cat("CIs of mu\n")
	mat <- 
		rbind(w$CI.exact, w$CI.basic, w$CI.percentile, w$CI.studentized)
	rownames(mat) <- 
		c("$CI.exact", "$CI.basic", "$CI.percentile", "$CI.studentized")
	print(mat)
}