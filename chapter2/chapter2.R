# The data file 'travels.xls' contains the original count data
# so first calculate the row profiles by dividing each row by its row total.

library(package="gdata")
library(package="rgl")
library(package="tcltk")

travels <- read.xls("travels.xls")
profiles <- matrix(0, ncol=3, nrow=4)
for (i in 1:length(t(travels[2]))) {
    for (j in 1:length(travels[i, -1])) {
        profiles[i,j] <- travels[i,-1][[j]]/sum(travels[i,-1])
    }
}

# Create the axies
rgl.lines(c(0,1.2), c(0,0), c(0,0))
rgl.lines(c(0,0), c(0,1.2), c(0,0))
rgl.lines(c(0,0), c(0,0), c(0,1.2))

rgl.lines(c(0,0), c(0,1), c(1,0), size=2)
rgl.lines(c(0,1), c(1,0), c(0,0), size=2)
rgl.lines(c(0,1), c(0,0), c(1,0), size=2)

rgl.points(profiles[,3], profiles[,1], profiles[,2], size=4)
rgl.texts(profiles[,3], profiles[,1], profiles[,2], row.names(travels))

# Button for saving snapshot
tkt <- tktoplevel()
label <- tklabel(tkt, text="For saving image")
button <- tkbutton(tkt, text="Save", command=function()rgl.snapshot(filename="chapter2.png"))
tkpack(label, button)
