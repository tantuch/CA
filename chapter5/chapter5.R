# Plotting Chi-square Distance

library(package="gdata")
library(package="rgl")

readers <- read.xls("readers.xls")
rownames(readers) <- split(readers[1][,1], row(readers[1]))
tab <- as.matrix(readers[-1])

# Find some chi-square statistic
tab.rowsum <- apply(tab, 1, sum)
tab.colsum <- apply(tab, 2, sum)
tab.sum <- sum(tab)

tab.pro <- tab/tab.rowsum
tab.colmass <- tab.colsum/tab.sum

# Find the weight for axies
tab.wt <- sqrt(tab.colmass)

# Create weighted axies
rgl.lines(c(0,1.2/tab.wt[2]), c(0,0), c(0,0))
rgl.lines(c(0,0), c(0, 1.2/tab.wt[3]), c(0,0))
rgl.lines(c(0,0), c(0,0), c(0,1.2/tab.wt[1]))

rgl.lines(c(0,0), c(0,1/tab.wt[3]), c(1/tab.wt[1],0), size = 2)
rgl.lines(c(0,1/tab.wt[2]), c(1/tab.wt[3],0), c(0,0), size = 2)
rgl.lines(c(0, 1/tab.wt[2]), c(0,0), c(1/tab.wt[1],0), size = 2)

# There are difference without transpose operation
tab.chi <- t(t(tab.pro)/tab.wt)
rgl.points(tab.chi[,2], tab.chi[,3], tab.chi[,1], size = 4)
rgl.texts(tab.chi[,2], tab.chi[,3], tab.chi[,1], text = row.names(tab))
