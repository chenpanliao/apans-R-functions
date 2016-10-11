library(dplyr)
library(parallel)
no_cores <- detectCores() - 0
N <- 1e7
means <- vector("list", N)

  f <- function(a){
    require(dplyr)
    x <- 1L:100L
    n <- 50
    n.sum <- 3
    m <- sample(x, n.sum * n, T)  %>%
      matrix(., 3, nrow = n, byrow = T) %>%
      apply(., 1, max) %>%
      mean
    return(m)
  }

cl <- makeCluster(no_cores)
x <- parSapply(cl, 1:N, FUN = f)
stopCluster(cl)
