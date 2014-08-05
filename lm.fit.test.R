# Copyright 2011 Chen-Pan Liao 
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

# Usage example:
#   model.name <- glm (...)
#   lm.fit.test(model.name)

# See http://apansharing.blogspot.tw/2011/08/function-of-pearsondeviance-goodness-of.html
# to learn how to use this function

lm.fit.test <- function (my.model) {
	df <- my.model$df.residual;
	chisq.pearson <- sum(resid(my.model , type="pearson")^2);
	chisq.deviance <- sum(resid(my.model , type="deviance")^2);
	p.pearson <- pchisq( chisq.pearson , df , lower.tail=F );
	p.deviance <- pchisq( chisq.deviance , df , lower.tail=F );
	ratio.pearson <- chisq.pearson / df;
	ratio.deviance <- chisq.deviance / df;
	cat(
		"Pearson chisq = " , chisq.pearson ,
		", df = " , df ,
		", p = " , p.pearson ,
		", chisq / df = " , chisq.pearson/df ,
		".\n" ,
		"Deviance chisq = " , chisq.deviance ,
		", df = " , df ,
		", p = " , p.deviance ,
		", chisq / df = " , chisq.deviance/df ,
		".\n"
	);
}