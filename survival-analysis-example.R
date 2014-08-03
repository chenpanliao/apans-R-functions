require(survival)

text <- textConnection("group	treat	body	predator	time	count
A	control	0.415	0	6.37	0
A	control	0.375	0	7.08	0
A	control	0.34	0	4.1	0
A	control	0.545	1	3.53	28.328611898
A	control	0.335	0	7	0
A	control	0.375	0	6.58	0
A	control	0.38	1	3.75	26.6666666667
A	control	0.395	0	7.98	0
A	control	0.475	0	8.02	0
A	control	0.34	0	4.1	0
A	control	0.414	0	7.87	0
A	control	0.404	0	7.37	0
A	control	0.365	0	7.43	0
A	control	0.329	0	7.45	0
A	control	0.428	0	7.48	0
A	control	0.338	0	7.53	0
A	control	0.422	0	6.57	0
A	control	0.331	0	6.47	0
A	control	0.348	0	7.02	0
A	control	0.568	0	7.23	0
A	control	0.301	0	7.22	0
A	control	0.376	0	7.08	0
A	control	0.38	0	7.48	0
A	control	0.326	0	7.9	0
A	control	0.375	0	7.37	0
A	control	0.324	0	6.38	0
A	control	0.332	0	6.75	0
A	control	0.31	0	7.15	0
A	control	0.301	0	3.57	0
B	spiderPainted	0.82	1	6.3	15.873015873
B	spiderPainted	0.35	0	7.7	0
B	spiderPainted	0.595	0	7.95	0
B	spiderPainted	0.336	0	7.82	0
B	spiderPainted	0.364	0	6.77	0
B	spiderPainted	0.33	0	7.2	0
B	spiderPainted	0.302	0	7.52	0
B	spiderPainted	0.401	0	7.4	0
B	spiderPainted	0.304	0	7.52	0
B	spiderPainted	0.42	0	7.45	0
B	spiderPainted	0.32	0	6.95	0
B	spiderPainted	0.41	0	6.95	0
B	spiderPainted	0.322	0	7.95	0
B	spiderPainted	0.35	0	7.92	0
B	spiderPainted	0.356	0	7.2	0
B	spiderPainted	0.37	0	7.08	0
B	spiderPainted	0.366	0	7.13	0
B	spiderPainted	0.36	0	7.05	0
C	decorationPainted	0.538	1	3.43	29.1545189504
C	decorationPainted	0.366	0	7.35	0
C	decorationPainted	0.31	0	7.18	0
C	decorationPainted	0.432	1	3.27	30.5810397554
C	decorationPainted	0.464	0	7.5	0
C	decorationPainted	0.302	1	2.43	41.1522633745
C	decorationPainted	0.302	0	7.02	0
C	decorationPainted	0.305	1	5.87	17.0357751278
C	decorationPainted	0.3	0	3.82	0
C	decorationPainted	0.42	0	6.35	0
C	decorationPainted	0.468	1	4.65	21.5053763441
C	decorationPainted	0.36	0	4.53	0
C	decorationPainted	0.319	1	3.95	25.3164556962
C	decorationPainted	0.408	0	7.9	0
C	decorationPainted	0.584	0	5.83	0
C	decorationPainted	0.301	0	7.47	0
C	decorationPainted	0.322	0	7.02	0
C	decorationPainted	0.371	0	7.23	0
C	decorationPainted	0.381	0	7.05	0
D	bothPainted	0.338	0	7.92	0
D	bothPainted	0.554	1	2.42	41.3223140496
D	bothPainted	0.344	0	7.3	0
D	bothPainted	0.49	0	7.43	0
D	bothPainted	0.31	0	7.43	0
D	bothPainted	0.318	0	6.4	0
D	bothPainted	0.399	0	7.27	0
D	bothPainted	0.302	0	7.2	0
D	bothPainted	0.368	0	7.13	0
D	bothPainted	0.402	0	4.12	0
D	bothPainted	0.47	0	7.38	0
D	bothPainted	0.328	0	8.2	0
D	bothPainted	0.506	0	7.9	0
D	bothPainted	0.368	0	4	0
D	bothPainted	0.37	0	7.12	0
D	bothPainted	0.37	0	6.58	0
D	bothPainted	0.402	0	4.817	0
D	bothPainted	0.478	0	1.6	0
")

dat0 <- read.table(text, header=T, sep="\t")
dat0$treatS <- NA
dat0$treatS[dat0$treat=="control"] <- "S+"
dat0$treatS[dat0$treat=="decorationPainted"] <- "S+"
dat0$treatS[dat0$treat=="bothPainted"] <- "S-"
dat0$treatS[dat0$treat=="spiderPainted"] <- "S-"
dat0$treatD <- NA
dat0$treatD[dat0$treat=="control"] <- "D+"
dat0$treatD[dat0$treat=="decorationPainted"] <- "D-"
dat0$treatD[dat0$treat=="bothPainted"] <- "D-"
dat0$treatD[dat0$treat=="spiderPainted"] <- "D+"
dat0$treatS <- as.factor(dat0$treatS)
dat0$treatD <- as.factor(dat0$treatD)
contrasts(dat0$treatD) <- contr.sum
contrasts(dat0$treatS) <- contr.sum

dat0$treat <- relevel(dat0$treat, ref="control")
dat0$rate <- dat0$predator / dat0$time
dat0 <- dat0[, -(which(names(dat0)=="count"))]
dat0 <- dat0[, -(which(names(dat0)=="group"))]

dat <- dat0
dat1 <- rbind(
  subset(dat, time>=6),
  subset(dat, time<6 & predator==1)
)
contrasts(dat1$treatD) <- contr.sum
contrasts(dat1$treatS) <- contr.sum



######### survival analysis
#msurv <- with(dat, Surv(time, predator==1))
#summary(msurv)
formu <- Surv(time, predator) ~ treat
fit <- survfit(formu, data = dat)
fit
summary(fit)
plot(fit, conf.int=F, mark.time=F, lty=2:5); legend(1,0.5,levels(dat$treat), lty=2:5)


# test 1
survdiff(formu, data = dat)

# test 2: Cox proportional hazards model
fit <- coxph(formu, data = dat)
fit
summary(fit)

fit1 <- coxph(Surv(time, predator) ~ treatS * treatD, data = dat)
fit1
summary(fit1)
fit1.step <- step(fit1)
summary(fit1.step)

# test 3
summary(survreg(formu, dist="exponential", data=dat))
summary(survreg(Surv(log(time), predator) ~ treat, dist="exponential", data=dat))
