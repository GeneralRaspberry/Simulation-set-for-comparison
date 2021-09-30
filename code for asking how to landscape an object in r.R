library(tidyr)
library(spatstat)

set.seed(23)

x<-runif(1000)*1000
y<-runif(1000)*1000
sim<-rep(1:10,100)

coordinates<-data.frame(x,y,sim)

coordinates<-coordinates[order(sim),]

emptylist<-vector(mode="list",length=10)
metriccalculation<-function(){
  emptylist<-ppp(x,y,owin(xrange=c(0,1000),yrange=c(0,1000)))
}

coordinates%>%group_by(sim)%>%lapply(coordinates,function(x)metriccalculation(data$x,data$y))
