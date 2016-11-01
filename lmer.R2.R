lmer.R2 <- function(mod) {
  # mod is a obj returns by lmer()
  var_fixed <- var(model.matrix(mod) %*% fixef(mod))
  var_random <- sum(sapply(VarCorr(mod), c))
  var_resid <- attr(VarCorr(mod), "sc") ^ 2
  r2_marginal <- var_fixed / (var_fixed + var_random + var_resid)
  r2_conditional <-
    (var_fixed + var_random) / (var_fixed + var_random + var_resid)
  ret <- c(
    R2.Marginal = as.vector(r2_marginal),
    R2.Conditional = as.vector(r2_conditional),
		SD.Fixed = sqrt(var(model.matrix(mod) %*% fixef(mod))),
		SD.Random = sqrt(sum(sapply(VarCorr(mod), c))),
		SD.Residual = attr(VarCorr(mod), "sc")
  )
  return(ret)
}
