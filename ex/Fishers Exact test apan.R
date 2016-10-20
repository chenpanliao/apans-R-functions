dt <- read.csv("data.csv")

# 假設你原本的 logistic regression 如下
m <- glm(is.eat ~ hook + hardness, data = dt, family = binomial)
summary(m)
# 這有問題，係數估計無法收斂

# 所以我們換個方式
# 首先，我創造一個變數 trt，是 hook 和 hardness 的組合
# 並進行 fisher exact test
dt$trt <- interaction(dt$hook, dt$hardness)
tabs <- xtabs( ~ is.eat + trt, data = dt)
fisher.test(tabs)
# 結果顯示四組 trt 的 odds 不全然相等
# 於是，我進行所有 trt 的兩兩比較，都是 fisher exact test，共做六次
pmat <- combn(levels(dt$trt), 2)
fisher.post <- vector("list", ncol(pmat))
for(i in 1:ncol(pmat)){
  thisInd <- 
    which(colnames(tabs) == pmat[1,i] | colnames(tabs) == pmat[2,i])
  fisher.post[[i]] <-
    fisher.test(tabs[, thisInd])
}
res <- sapply(fisher.post, function(x){
  list(
    odds.ratio = x$estimate,
    p.value = x$p.value
  )
})
colnames(res) <- apply(pmat, 2, function(x) paste(x, collapse = " vs "))
# 因為這是 multiple test，所以進行 Bonferroni correction（即 alpha = 0.05 / 6）
res <- rbind(res, p.value.adjusted = p.adjust(res["p.value", ], "bonferroni"))
# 顯示兩兩比較的結果
print(res)
# 如果把上述結果看懂，你會發現四組 trt 的 odds ratio 可顯著地分成二群
# odds ratio 大的一群是 "no hook.hard" 和 "hook.hard"
# odds ratio 小的一群是 "no hook.soft" 和 "hook.soft"
# 換句話說，hook 是顯著因子，hardness 不是
# 完工
