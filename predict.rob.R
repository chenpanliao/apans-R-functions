## find confidence band with sandwich correction for linear model
predict.rob <-
  function(x, clcov, newdata, df, useVCOVHC = T) {
    require(sandwich)
    
    if (missing(newdata)) {
      newdata <- x$model
    }
    if (missing(df)) {
      df <- summary(x)$df[2]
    }
    
    tt <- terms(x)
    Terms <- delete.response(tt)
    m.mat <- model.matrix(Terms, data = newdata)
    m.coef <- x$coef
    fit <- as.vector(m.mat %*% x$coef)
    se.fit <- sqrt(diag(m.mat %*% clcov %*% t(m.mat)))
    lwr <- fit - qt(0.975, df) * se.fit
    upr <- fit + qt(0.975, df) * se.fit
    data.frame(fit, se.fit, lwr, upr)
  }
