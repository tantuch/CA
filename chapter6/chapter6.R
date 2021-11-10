# Reduction of Dimensionality

library(package="gdata")
library(package="rgl")

# To prepare table of health
health <- read.xls("health.xls")
rownames(health) <- split(health[,1], row(health[1]))
health <- health[-1]

# To find weighted values
health.P <- health/sum(health)

# Total values and some preparation
health.r <- apply(health.P, 1, sum)
health.c <- apply(health.P, 2, sum)
health.Dr <- diag(health.r)
health.Dc <- diag(health.c)
health.Drmh <- diag(1/sqrt(health.r))
health.Dcmh <- diag(1/sqrt(health.c))

# To calculate Standarized Residuals and SVD
health.P <- as.matrix(health.P)
health.S <- health.Drmh%*%(health.P - health.r%o%health.c)%*%health.Dcmh
health.svd <- svd(health.S)

# To get standard coordinates
health.rsc <- health.Drmh%*%health.svd$u
health.csc <- health.Dcmh%*%health.svd$v

# To get principal coordinates
health.rpc <- health.rsc%*%diag(health.svd$d)
health.cpc <- health.csc%*%diag(health.svd$d)

plot(health.rpc[,1], health.rpc[,2], type = "n", pty = "s")
text(health.rpc[,1], health.rpc[,2], label = rownames(health))
