# Copyright 2012 Chen-Pan Liao 
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
# along with this program.	 If not, see <http://www.gnu.org/licenses/>.

# Author: Chen-Pan Liao
# Date: May 28, 2012
#
# This work is inspirated by Dr. David C. Howell, University of Vermont.
# See Dr. Howell's webpage to learn more:
# http://www.uvm.edu/~dhowell/StatPages/More_Stuff/
# Permutation%20Anova/PermTestsAnova.html

# See http://apansharing.blogspot.tw/2012/05/nested-anova-permutation-test-in-r.html
# to learn how to use this function

nestedPermutationAnova <- function(
	dv,		# Dependent variable
	treatment,	# Treatment
	subject,	# Experimental unit
	nreps = 1999	# Number of permutation
) {
	# initiation
	dv <- as.numeric(dv)
	treatment <- as.factor(treatment)
	subject <- as.factor(subject)
	nreps <- as.integer(nreps)
	subject <- as.factor(paste(subject,treatment,sep="_in_"))
		# rename subject as "subject_in_treatment"
	treatment.name <- levels(treatment)
	subject.name <- levels(subject)
	treatment.n <- nlevels(treatment)
	subject.n <- nlevels(subject)
	
	f.treatment <- numeric(nreps)
	f.subject <- numeric(nreps);
	
	# original model
	model.0 <- summary(aov(dv ~ treatment / subject))
	f.subject.0 <- model.0[[1]][1,4]
	f.treatment.0 <- model.0[[1]][1,3] / model.0[[1]][2,3]
	
	# permutation
	for (i in 1:nreps) {

		# permutation for effect of subject
		dv.subject <- NULL
		for (j in 1:treatment.n){
			dv.subject <- c(
				dv.subject,
				sample(dv[ treatment == treatment.name[j] ])
			)
		}
		model <- summary(aov(dv.subject ~ treatment / subject))
		f.subject[i] <- model[[1]][1,4]
		
		# permutation for effect of treatment
		dv.treatment <- NULL
		subject.name.reorder <- sample(subject.name)
		for (j in 1:subject.n){
			dv.treatment <- c(
				dv.treatment,
				dv[ subject == subject.name.reorder[j] ]
			)
		}
		model <- summary(aov(dv.treatment ~ treatment / subject))
		f.treatment[i] <- model[[1]][1,3] / model[[1]][2,3]
	}
	
	# p-value
	p.treatment <- (sum(f.treatment >= f.treatment.0)+1) / (nreps+1)
	p.subject <- (sum(f.subject >= f.subject.0)+1) / (nreps+1)
	
	# plot
	par(mfrow = c(1,2))
	hist(f.treatment, breaks=40, xlab="Sudo f-value for treatment", main="")
	abline(v=f.treatment.0, lty=2)
	hist(f.subject, breaks=40, xlab="Sudo f-value for subject", main="")
	abline(v=f.subject.0, lty=2)
	
	# return
	return (
		list(
			p.treatment = p.treatment,
			p.subject = p.subject,
			nreps = nreps
		)
	)
}