# y_ij = mu + tau_i + beta_j + epsilon_ij

set.seed(123)
N <- 100
mu <- 50
tau <- c(-5, 5)
beta <- rnorm(N, 0, 10)
sigma <- 0.1

y1 <- mu + tau[1] + beta + rnorm(N, 0, sigma)
y2 <- mu + tau[2] + beta + rnorm(N, 0, sigma)
dt <- data.frame(
  y = c(y1, y2),
  trt = gl(2, N),
  block = gl(N, 1, N * 2)
)
contrasts(dt$trt) <- c(-1, 1)


t.test(y1, y2, paired = T)

library(lme4)
summary(lmer(y ~ trt + (1|block), data = dt))



library(MCMCglmm)

ptm <- proc.time()
my.prior <- list(
  B = list(mu = c(0, 0), V = diag(2) * 100000000),
  G = list(G1 = list(V = 1, nu = 0.0002)),
  R = list(V = 1, nu = 0.0002)
)
fit <- MCMCglmm(
  y ~ trt, random = ~ block, data = dt,
  family = "gaussian", prior = my.prior, rcov = ~ units,
  nitt = 100000, thin = 10, burnin = 50000,
  pr = TRUE, pl = TRUE
)
proc.time() - ptm


summary(fit)
