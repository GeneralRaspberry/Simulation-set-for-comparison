library(tidyr)
library(spatstat)

set.seed(23)

x<-runif(1000)*1000
y<-runif(1000)*1000
sim<-rep(1:10,100)

coordinates<-data.frame(x,y,sim)

coordinates<-coordinates[order(sim),]

coord_list<-split(coordinates[,c("x","y")],coordinates$sim)
ppplist<-lapply(coord_list,as.ppp,W=square(1000))



