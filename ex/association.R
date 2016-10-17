#### P.112 二維的獨立性檢驗
txt <- "Freq,Neighborhood,Party
221,B,D
160,H,D
360,L,D
140,S,D
200,B,I
291,H,I
160,L,I
311,S,I
208,B,R
106,H,R
316,L,R
97,S,R"

dat <- read.csv(textConnection(txt))
dat.tab <- xtabs(Freq ~ Neighborhood + Party, data = dat)

## Pearson's Chi-square test
chisq.test(dat.tab)
summary(dat.tab)

## Likelihood ration Chi-square test (G-test)
library(MASS)
loglm(Freq ~ Neighborhood + Party, data = dat)

#### P.119 Mantel-Haenszel test
#### 以 Hospital 分層
#### 檢驗 Trt 和 YN 是否有關聯性
txt <- "Hospital,Trt,Freq,YN
A,a,29,Y
A,a,16,N
A,b,14,Y
A,b,31,N
B,a,37,Y
B,a,8,N
B,b,24,Y
B,b,21,N"

dat <- read.csv(textConnection(txt))
dat.tab <- xtabs(Freq ~ Trt + YN + Hospital, data = dat)
mantelhaen.test(dat.tab) # 須在上一步中 Hospital 放最後


#### P.129 CMH test
#### 以 School 分層
#### Program 和 Learning 皆為 nomial
#### 檢驗 Program 和 Learning 是否有關聯性
txt <- "School,Program,Learning,Freq
a,r,s,10
a,r,t,17
a,r,c,26
a,a,s,5
a,a,t,12
a,a,c,50
b,r,s,21
b,r,t,17
b,r,c,26
b,a,s,16
b,a,t,12
b,a,c,36
c,r,s,15
c,r,t,15
c,r,c,16
c,a,s,12
c,a,t,12
c,a,c,20
"
dat <- read.csv(textConnection(txt))
dat.tab <- xtabs(Freq ~ Program + Learning + School, data = dat)
mantelhaen.test(dat.tab) # 須在上一步中 School 放最後


#### P. 131 CMH test
#### 以 Area:Location 分層
#### 檢驗 CarSize 和 YN 的關聯性
#### CarSize 為 ordinal，YN 為 nomial
txt <- "Area Location CarSize YN Freq
coast urban large no 174
coast urban large yes 69
coast urban medium no 134
coast urban medium yes 56
coast urban small no 150
coast urban small yes 54
coast rural large no 52
coast rural large yes 14
coast rural medium no 31
coast rural medium yes 14
coast rural small no 25
coast rural small yes 17
pidemont urban large no 127
pidemont urban large yes 62
pidemont urban medium no 94
pidemont urban medium yes 63
pidemont urban small no 112
pidemont urban small yes 93
pidemont rural large no 35
pidemont rural large yes 29
pidemont rural medium no 32
pidemont rural medium yes 30
pidemont rural small no 46
pidemont rural small yes 34
mountain urban large no 111
mountain urban large yes 26
mountain urban medium no 120
mountain urban medium yes 47
mountain urban small no 145
mountain urban small yes 68
mountain rural large no 62
mountain rural large yes 31
mountain rural medium no 44
mountain rural medium yes 32
mountain rural small no 85
mountain rural small yes 43
"
dat <- read.table(textConnection(txt), sep=" ", header=T)
dat$AreaLocation <- paste0(dat$Area, dat$Location)
dat.tab <- xtabs(Freq ~ CarSize + YN + AreaLocation, data = dat)

library(vcdExtra)
CMHtest(Freq ~ CarSize + YN | Area + Location, overall=T, data = dat, cscores = "midrank")$ALL
CMHtest(dat.tab, overall=T, strata="AreaLocation", cscores="midrank")$ALL
#自由度不對？


#### P.133 CMH
#### Hospital 為分層
#### 檢驗 Symptoms 和 Operation 的關聯（二者皆為 ordinal）
txt <- "Hospital Operation Symptoms Freq
1 v+d none 23
1 v+d slight 7
1 v+d moderate 2
1 v+a none 23
1 v+a slight 10
1 v+a moderate 5
1 v+h none 20
1 v+h slight 13
1 v+h moderate 5
1 gre none 24
1 gre slight 10
1 gre moderate 6
2 v+d none 18
2 v+d slight 6
2 v+d moderate 1
2 v+a none 18
2 v+a slight 6
2 v+a moderate 2
2 v+h none 13
2 v+h slight 13
2 v+h moderate 2
2 gre none 9
2 gre slight 15
2 gre moderate 2
3 v+d none 8
3 v+d slight 6
3 v+d moderate 3
3 v+a none 12
3 v+a slight 4
3 v+a moderate 4
3 v+h none 11
3 v+h slight 6
3 v+h moderate 2
3 gre none 7
3 gre slight 7
3 gre moderate 4
4 v+d none 12
4 v+d slight 9
4 v+d moderate 1
4 v+a none 15
4 v+a slight 3
4 v+a moderate 2
4 v+h none 14
4 v+h slight 8
4 v+h moderate 3
4 gre none 13
4 gre slight 6
4 gre moderate 4
"
dat <- read.table(textConnection(txt), sep=" ", header=T)
dat$Hospital <- as.factor(dat$Hospital)
dat.tab <- xtabs(Freq ~ Operation + Symptoms + Hospital, data = dat)
library(vcdExtra)
#CMHtest(dat.tab, overall=T, strata="Hospital", cscores="midrank", rscores="midrank")$ALL
CMHtest(dat.tab, overall=T, strata="Hospital",  rscores=c(4,2,1,3), cscores=c(3,1,2))$ALL
#不完全相同於課本

#### P.122 檢驗二組的 odds ratio 是否相等
txt <- "Gender,ECG,Freq,YN
F,L,8,Y
F,L,10,N
F,S,4,Y
F,S,11,N
M,L,21,Y
M,L,6,N
M,S,9,Y
M,S,9,N"

dat <- read.csv(textConnection(txt))
dat.tab <- xtabs(Freq ~ YN + ECG + Gender, data = dat)
######################################################################
# Function to perform the Breslow and Day (1980) test including
# the corrected test by Tarone
# Uses the equations in Lachin (2000) p. 124-125.
#
# Programmed by Michael Hoehle <http://www-m4.ma.tum.de/pers/hoehle>
# Note that the results of the Tarone corrected test do
# not correspond to the numbers in the Lachin book...
#
# Params:
#  x - a 2x2xK contingency table
#
# Returns:
#  a vector with three values
#   1st value is the Breslow and Day test statistic
#   2nd value is the correct test by Tarone
#   3rd value - p value based on the Tarone test statistic
#               using a \chi^2(K-1) distribution
######################################################################

breslowday.test <- function(x) {
  #Find the common OR based on Mantel-Haenszel
  or.hat.mh <- mantelhaen.test(x)$estimate
  #Number of strata
  K <- dim(x)[3]
  #Value of the Statistic
  X2.HBD <- 0
  #Value of aj, tildeaj and Var.aj
  a <- tildea <- Var.a <- numeric(K)

  for (j in 1:K) {
    #Find marginals of table j
    mj <- apply(x[,,j], MARGIN=1, sum)
    nj <- apply(x[,,j], MARGIN=2, sum)

    #Solve for tilde(a)_j
    coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
                 1-or.hat.mh)
    sols <- Re(polyroot(coef))
    #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
    #Observed value
    aj <- x[1,1,j]

    #Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj

    #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)

    #Compute contribution
    X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)

    #Assign found value for later computations
    a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
  }

  #Compute Tarone corrected test
  X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )

  #Compute p-value based on the Tarone corrected test
  p <- 1-pchisq(X2.HBDT, df=K-1)

  res <- list(X2.HBD=X2.HBD,X2.HBDT=X2.HBDT,p=p)
  class(res) <- "bdtest"
  return(res)
}

print.bdtest <- function(x) {
  cat("Breslow and Day test (with Tarone correction):\n")
  cat("Breslow-Day X-squared         =",x$X2.HBD,"\n")
  cat("Breslow-Day-Tarone X-squared  =",x$X2.HBDT,"\n\n")
  cat("Test for test of a common OR: p-value = ",x$p,"\n\n")
}

breslowday.test(dat.tab)



txt <- "Gender,ECG,FY, FN
F,L,8,10
F,S,4,11
M,L,21,6
M,S,9,9"
dat <- read.csv(textConnection(txt))
summary(glm(cbind(FY,FN) ~ Gender*ECG, data=dat, family=binomial, contrasts=list(Gender=c(0.5,-0.5),ECG=c(0.5,-0.5))))
# glm() 的交互作用項和上面的 breslowday.test() 結果一致。
# breslowday.test() 直接檢驗 F 的 odds ratio 是否和 M 的 odds ratio 相等。
