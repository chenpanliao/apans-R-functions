require(magrittr)
require(msm)
reg.CI <-
  function(model,
           reg.formula.str = "~ x1 + %f * x2",
           nameX = "X",
           newX = seq(0, 9)) {
    form <- sprintf(reg.formula.str, newX)
    se <- vector("numeric", length(newX))
    for (i in 1:length(newX)) {
      se[i] <- deltamethod(as.formula(form[i]), coef(model), vcov(model))
    }
    DF =
      if (class(model) == "lm") {
        model$df.residual
      } else if (class(model) == "nls") {
        model %>% summary %>% .$df %>% .[2]
      }
    newData <- data.frame(newX) %>% set_colnames(nameX)
    predict.newData <- predict(model, newData)
    upper <- predict.newData + se * qt(0.975, DF)
    lower <- predict.newData - se * qt(0.975, DF)
    cbind(newX,
          newY = predict.newData,
          lower,
          upper)
  }
