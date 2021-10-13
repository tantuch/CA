# Masses and Centroids

library(package="gdata")

readers <- read.xls("readers.xls")
# To name rows for imported list "readers" before creation visual matrix
rownames(readers) <- split(readers[1][,1], row(readers[1]))

# To create matrix with rownames and columnnames
tab <- as.matrix(readers[-1])

# To get rows-profiles(1 indicates rows for apply-function) for all elements in matrix
tab.pro <- tab / apply(tab, 1, sum)

# To create triangle with column-vertises
plot.new()
lines(c(0, 1, 0.5, 0), c(0, 0, sqrt(3)/2, 0), col="gray")
text(c(0, 1, 0.5), c(0, 0, sqrt(3)/2), labels=colnames(tab))

# To recalculate coordinates of points in two-dimensional triangle space
tab.x <- 1 - tab.pro[,1] - tab.pro[,3]/2
tab.y <- tab.pro[,3]*sqrt(3)/2
text(tab.x, tab.y, labels=rownames(tab))

# To point position of centroid
tab.cx <- 1 - tab.colmass[1] - tab.colmass[3]/2
tab.cy <- tab.colmass[3]*sqrt(3)/2
text(tab.cx, tab.cy, labels="C*")
