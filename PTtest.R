# 寫入 data.frame 的多型
rm(list = ls())


#opi <- c("a", "b", "c", "d")
#x <- as.table(matrix(c(144, 33, 84, 126, 
#                         2,  4, 14,  29, 
#                         0,  2,  6,  25, 
#                         0,  0,  1,  5),
#              nrow = 4, 
#              dimnames = list(Pre = opi,
#                              Post = opi)))
#source("~/desktop/PTtest.R");results <- PTtest(x);summary(results);plot(results)

#x <- matrix(
#  c(144, 33, 84, 126, 
#      2,  4, 14,  29, 
#      0,  2,  6,  25, 
#      0,  0,  1,  5),
#  nrow = 4
#)
#source("~/desktop/PTtest.R");results <- PTtest(x);summary(results);plot(results)

#test11 <- gl(5, 1, 25)
#test22 <- gl(5, 5, 25)
#freq = round(runif(25, 10, 50))
#rawdata2 <- data.frame(test11, test22, freq)
#source("~/desktop/PTtest.R"); results <- PTtest(freq ~ test11 + test22, data = rawdata2);summary(results);plot(results)

#rawdata1 <- data.frame(
#  pre=sample(c("a","b","c","d","e"),200,1),
#  pos=sample(c("a","b","c","d","e"),200,1)
#)
#source("~/desktop/PTtest.R"); results <- PTtest( ~ pre + pos, data = rawdata1);summary(results);plot(results)














############################################
## File: PTtest.R
## Description: The function PTtest()
##              performs Stuart-Maxwell test, Bhapkar's test
##              and Bowker's test and their permutation tests.
## Author: Chen-Pan Liao (2013)
## License: Public domain
## Environment: R (ver. 2+)
## Usage:
##   > results <- PTtest(x);
##   > summary(results);
##   > plot(results)
##   where x must be a square matrix
##   which contains more than two row/column.
##   See
##     http://apansharing.blogspot.com/2013/09/
##     tests-for-marginal-homogeneity-and.html
##   to learn more.
## Reference: 
##    Bhapkar VP. A note on the equivalence of two test criteria
##      for hypotheses in categorical data. Journal of the American 
##      Statistical Association, 1966, 61, 228-235.
##    Bowker AH. Bowker’s Test for Symmetry. Journal of the American
##      Statistical Association, 1948, 43, 572–574.
##    Maxwell AE. Comparing the classification of subjects by two
##      independent judges. British Journal of Psychiatry, 1970,
##      116, 651-655.
##    Stuart AA. A test for homogeneity of the marginal distributions
##      in a two-way classification. Biometrika, 1955, 42, 412-416.
##    
############################################

# constructor
PTtest <- function (x, ...){UseMethod("PTtest")}

# data.frame function
PTtest.formula <- function (
  x,             # formula
  data,          # data.frame
  B = 1999,      # number of permutation
  ...
) {
  cat("CALL .formula \n")
  x <- xtabs(x, data = data)
  NextMethod("PTtest")
}

# matrix/table/xtabs/default function
PTtest.default <- function(
  x,             # a square matrix with 3+ row/column
  B = 1999,      # number of permutation
  ...
) {
  cat("CALL .default \n")
  
  # check: x is matrix/table/xtabs
  if( !any(class(x) %in% c("matrix", "table", "xtabs")) ) {
    stop("class of ", x.name, " must be matrix, table or xtabs.")
  }

  # check: x is two-dimensional
  if (length(dim(x)) != 2) {
    stop(x.name, " must be a two-dimensional matrix.")
  }
  
  # check: x is a square matrix
  if (dim(x)[1] != dim(x)[2]) {
    stop(x.name, " must be a square table.")
  }

  # check: level >= 3
  if (dim(x)[1] < 3) {
    warning(
      "number of row and column of ",
      x.name,
     " should be larger than 2.\n Use McNemar's test for 2 x 2 table."
    )
  }

  # check: nonnegative integer
  is.interger <- abs(x - round(x)) < .Machine$double.xmin
  is.nonnegative <- x >= 0
  if ( !(all(is.interger) && all(is.nonnegative)) ) {
    stop("all items in x must be nonnegative integer.")
  }

  # check: level names and test names
  x.name <- deparse(substitute(x))
  LEVEL <- dim(x)[1]
  levelNames <- dimnames(x)[[1]]
  testNames <- c(names(dimnames(x)[1]), names(dimnames(x)[2]))
  if ( any(is.null(testNames)) || any(testNames == "") ) {
    testNames <- c("Test.1", "Test.2")
    x <- as.table (
      matrix (
        as.numeric (x), nrow = LEVEL, ncol = LEVEL, 
        dimnames = list(Test.1 = levelNames, Test.2 = levelNames)
      )
    )
  }

  # chi-square
  BowkerStatistic <- function(xx){
    dat.mat <- (xx - t(xx))^2 / (xx + t(xx) + .Machine$double.xmin)
    return(sum(dat.mat[upper.tri(dat.mat)]))
  }
  StuartMaxwellStatistic <- function(xx){
    S <- -(xx + t(xx))
    diag(S) <- rowSums(xx) + colSums(xx) - 2*diag(xx)
    keep <- 1:(nrow(xx)-1)
    d <- rowSums(xx) - colSums(xx)
    sol <- t(d[keep]) %*% solve(S[keep, keep]) %*% d[keep]
    return(sol[1])
  }
  BhapkarStatistic <- function(xx){
    S <- -(xx + t(xx))
    diag(S) <- rowSums(xx) + colSums(xx) - 2*diag(xx)
    keep <- 1:(nrow(xx)-1)
    d <- rowSums(xx) - colSums(xx)
    sol <- t(d[keep]) %*% solve(S[keep, keep]) %*% d[keep]
    return(sol[1] / (1 - sol[1] / sum(xx)))
  }
  STA.StuartMaxwell <- StuartMaxwellStatistic(x)
  STA.Bhapkar <- BhapkarStatistic(x)
  STA.Bowker <- BowkerStatistic(x)

  # permutation
  # 對角線不變而對應二端p=0.5的二項分配，同等於每個樣本的前後測結果隨機交換
  permuteTP <- function(xx) {
    xx.upper <- xx[upper.tri(xx)]
    xx.lower <- t(xx)[upper.tri(t(xx))]
    xx.diag <- diag(xx)
    xx.nondiag.sum <- xx.upper + xx.lower
    LEVEL <- dim(xx)[1]
    item.n <- LEVEL * (LEVEL - 1) / 2
    xx.upper.perm <- rbinom(item.n, xx.nondiag.sum, 0.5)
    xx.lower.perm <- xx.nondiag.sum - xx.upper.perm
    xx.perm <- matrix(0E1, LEVEL, LEVEL)
    xx.perm[upper.tri(xx.perm)] <- xx.lower.perm
    xx.perm <- t(xx.perm)
    diag(xx.perm) <- xx.diag
    xx.perm[upper.tri(xx.perm)] <- xx.upper.perm
    return(xx.perm)
  }
  STA.StuartMaxwell.perm <- numeric(B)
  STA.Bhapkar.perm <- numeric(B)
  STA.Bowker.perm <- numeric(B)
  for(j in 1:B) {
    x.perm <- permuteTP(x)
    STA.StuartMaxwell.perm[j] <- StuartMaxwellStatistic(x.perm)
    STA.Bhapkar.perm[j] <- BhapkarStatistic(x.perm)
    STA.Bowker.perm[j] <- BowkerStatistic(x.perm)
  }
  
  # return
  out <- list (
    freqTable = x,
    levelNames = levelNames,
    testNames = testNames,
    levels = LEVEL,
    subjects = sum(x),
    statistic.stuart.maxwell = STA.StuartMaxwell,
    statistic.bhapkar = STA.Bhapkar,
    statistic.bowker = STA.Bowker,
    statistic.stuart.maxwell.permute = STA.StuartMaxwell.perm,
    statistic.bhapkar.permute = STA.Bhapkar.perm,
    statistic.bowker.permute = STA.Bowker.perm,
    df.stuart.maxwell = LEVEL - 1,
    df.bhapkar = LEVEL - 1,
    df.bowker = LEVEL * (LEVEL - 1) / 2,
    p.value.stuart.maxwell = 1 - pchisq(STA.StuartMaxwell, LEVEL - 1),
    p.value.bhapkar = 1 - pchisq(STA.Bhapkar, LEVEL - 1),
    p.value.bowker = 1 - pchisq(STA.Bowker, LEVEL * (LEVEL - 1) / 2),
    p.value.stuart.maxwell.perm = (
      sum(STA.StuartMaxwell.perm >= STA.StuartMaxwell) + 1) / (B + 1
    ),
    p.value.bhapkar.perm = 
      (sum(STA.Bhapkar.perm >= STA.Bhapkar) + 1) / (B + 1),
    p.value.bowker.perm = 
      (sum(STA.Bowker.perm >= STA.Bowker) + 1 ) / (B + 1),
    permutation.n = B
  )
  class (out) <- "PTtest"
  return (out)
}


## summary function
summary.PTtest <- function(
  x, digits = 6
){
  cat("\n")
  print(addmargins(x$freqTable))
  cat("\n")
  cat("Number of levels:", x$levels, "\n")
  cat("Level:", paste(x$levelNames, collapse = ", "), "\n")
  cat("Tests:", paste(x$testNames, collapse = ", "), "\n")
  cat("Number of subjects:", x$subjects, "\n")
  cat("Number of consistent cases:", sum(diag(x$freqTable)), "\n")
  cat(
    "Number of inconsistent cases:", 
    x$subjects - sum(diag(x$freqTable)),
    "\n"
  )
  cat("Number of permutations:", x$permutation.n, "\n")
  cat("\n")
  cat("== Marginal proportions ==", "\n")
  mat <-
    matrix(
    c(
      rowSums(x$freqTable)/sum(x$freqTable),
      colSums(x$freqTable)/sum(x$freqTable)
    ),
    nrow = 2,
    byrow = T
  )
  rownames(mat) <- x$testNames
  colnames(mat) <- x$levelNames
  print(mat, digits = digits)
  cat("\n")
  cat("== Tests for marginal homogeneity (MH) and symmetry (S) ==", "\n")
  mat <- matrix(
    c(
      x$statistic.stuart.maxwell,
      x$statistic.bhapkar,
      x$statistic.bowker,
      x$df.stuart.maxwell,
      x$df.bhapkar,
      x$df.bowker,
      x$p.value.stuart.maxwell,
      x$p.value.bhapkar,
      x$p.value.bowker,
      x$p.value.stuart.maxwell.perm,
      x$p.value.bhapkar.perm,
      x$p.value.bowker.perm
    ),
    nrow = 3,
    dimnames = list(
      c(
        "Stuart-Maxwell test for MH",
        "Bhapkar's test for MH",
        "Bowker's test for S"
      ),
      c(
        "statistic",
        "  df",
        "  p",
        "  permuted.p"
      )
    )
  )
  print(mat, digits = digits)
  cat("\n")
}

# plot function
plot.PTtest <- function(x){
  dev.new(width=8, height=6)
  layout(matrix(c(1,2), 2, 1, byrow = T), widths=c(1,1), heights=c(1,1))
  par(mar=c(5, 5, 5, 8), cex = 0.75, xpd = T)

  # marginal bar plot
  barplot(
    mat <- matrix(
      c(
        rowSums(x$freqTable)/sum(x$freqTable),
        colSums(x$freqTable)/sum(x$freqTable)
      ),
      nrow = x$levels, 
      byrow = F
    ), 
    horiz = T,
    beside = F,
    names.arg = x$testNames,
    legend.text = x$levelNames,
    args.legend = list(x = "right", legend = x$levelNames, inset=c(-0.15,0)),
    main = "Marginal proportions"
  )

  # inconsistent bar plot
  xx <- x$freqTable
  xx.upper <- xx[upper.tri(xx)]
  xx.lower <- t(xx)[upper.tri(t(xx))]
  xx.non.diag <- xx.upper + xx.lower
  xx.diag <- diag(xx)
  xx.names <- dimnames(xx)
  xx.names.combine <- character(x$levels * (x$levels - 1) / 2)
  k <- 1
  for (j in 1:x$levels) {
    for (i in 1:x$levels) {
      if (i < j) {
        xx.names.combine[k] <- paste(
          xx.names[[1]][i], xx.names[[2]][j], sep=","
        )
        k <- k + 1
      }
    }
  }
  xx.names.combine <- paste("{", xx.names.combine, "}", sep="")
  barplot(
    mat <- matrix(
      c(xx.upper, xx.lower),
      nrow=2, 
      byrow=T, 
      dimnames = list(
        Test = x$testNames,
        Freq = xx.names.combine
      )
    ), 
    beside = T,
    names.arg = xx.names.combine,
    legend.text = rownames(mat),
    args.legend = list(x = "right", inset=c(-0.15,0)),
    main = "Frequency of inconsistent cases",
    ylab = "Frequency"
  )
}
