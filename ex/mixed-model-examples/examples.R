library(MCMCglmm)
library(lmerTest)

set.seed(999)
trt <- gl(3, 1, 60)
contrasts(trt) <- contr.sum
subject <- gl(10, 6)
y <- as.numeric(trt) + as.numeric(subject) + rnorm(60)
dt <- data.frame(trt, subject, y)

aov_RCBD <- aov(y ~ Error(subject) + trt, data = dt)
summary(aov_RCBD)

lmer_RCBD <- lmerTest::lmer(y ~ trt + (1|subject), data = dt)
summary(lmer_RCBD)
car::Anova(lmer_RCBD, type = 3, test.statistic = "F")

lme_RCBD <- nlme::lme(y ~ trt, random = ~ 1|subject, data = dt)
summary(lme_RCBD)
anova(lme_RCBD)


ptm <- proc.time()
mcmcglmm_RCBD <- MCMCglmm::MCMCglmm(
  y ~ trt, random = ~ subject, data = dt, family = "gaussian",
  prior = list(
    B = list(mu = rep(0, 3), V = diag(3) * 1e6),
    G = list(G1 = list(V=1, nu=0.002)),
    R = list(V=1, nu=0.002)
  )
)
proc.time() - ptm

summary(mcmcglmm_RCBD)
plot(mcmcglmm_RCBD)
