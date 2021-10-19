# Chi-square Distance and Inertia

library(package="gdata")
tab <- read.xls("readers.xls")
readers <- as.matrix(tab[-1])

# calculate chi-square statistic
readers.rowsum <- apply(readers, 1, sum)
readers.colsum <- apply(readers, 2, sum)
readers.sum <- sum(readers)
readers.exp <- readers.rowsum%o%readers.colsum/readers.sum
chi2 <- sum((readers - readers.exp)^2/readers.exp)

# calculate chi-square distance to centroid
readers.colmass <- readers.colsum/readers.sum
readers.pro <- readers/apply(readers, 1, sum)

# example for one point
chidist <- 0
for(j in 1:dim(readers)[2]) {
    chidist <- chidist + (readers.pro[5, j] - readers.colmass[j])^2/readers.colmass[j]
}

# example of extraction the distance to centroid for all values
to_centroid <- apply((t(readers.pro) - readers.colmass)^2/readers.colmass, 2, sum)

# convenient way to achive chi-squre distance for all pairs in table
readers.pro <- rbind(readers.pro, readers.colmass)
rownames(readers.pro)[6] <- "ave"
between_all_pairs <- dist(sweep(readers.pro, 2, sqrt(readers.colmass), FUN = "/"))
