## dbRDA 

require(vegan)

# 多樣性資料
data(dune)
dune

# 環境資料
data(dune.env)
dune.env

# 環境資料標準化
dune.env.std <- dune.env
dune.env.std$A1 <- decostand(dune.env[,1] , method="standardize", MARGIN=2)

# dbRDA
dbRDA.fit <- capscale(dune ~ A1+Moisture+Management+Use+Manure , data=dune.env, dist="bray")
summary(dbRDA.fit)

dbRDA.fit.std <- capscale(dune ~ A1+Moisture+Management+Use+Manure , data=dune.env.std, dist="bray")
summary(dbRDA.fit.std)

par(mfrow=c(2,2))
plot(dbRDA.fit, scaling=1)
plot(dbRDA.fit, scaling=2)

# partition of variation
varpart.fit <- varpart(dune, ~A1+Moisture+Manure+Use, ~Management, data=dune.env.std, transfo="norm", scale = T)
showvarparts(2)
varpart.fit
plot(varpart.fit)
