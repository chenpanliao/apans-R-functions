N <- 5
B <- 10000
M.S.tf.sum <- matrix(0.0, B, N^2)

for(j in 1:B) {
    S <- sample(1L:(N^2))
    M <- matrix(F, N, N)
    M.S <- list()
    for(i in 1 : length(S)){
      M.S[[i]] <- M
      M.S[[i]][S[1L:i]] <- T
    }
    
    M.S.tf <- lapply(
      M.S,
      function(x){
        cs <- colSums(x)
        rs <- rowSums(x)
        ds <- sum(diag(x))
        ads <- sum(diag(apply(x, 1, rev)))
        return( c(cs, rs, ds, ads)/5 == 1)
      }
    )
    
    # how many bingo
    M.S.tf.sum[j,] <- sapply( (lapply(M.S.tf, sum)) , '[[' , 1)
}


# 至少1線的CMF
colSums(M.S.tf.sum >= 1)/B
# 至少5線的CMF
colSums(M.S.tf.sum >= 5)/B