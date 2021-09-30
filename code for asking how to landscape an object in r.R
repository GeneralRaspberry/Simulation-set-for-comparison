
library(spatstat)

set.seed(23)

x<-runif(1000)*1000
y<-runif(1000)*1000
sim<-rep(1:10,100)

coordinates<-data.frame(x,y,sim)

coordinates<-coordinates[order(sim),]

coord_list<-split(coordinates[,c("x","y")],coordinates$sim)
ppplist<-lapply(coord_list,as.ppp,W=square(1000))

kestlist<-list()
MetricAverage<-c()
for (i in (1:length(ppplist))){
  kk<-Kest(ppplist[[i]])
  kk_iso<-kk$iso
  kk_pois<-kk$theo
  
  kk_div_na<-kk_iso/kk_pois
  kk_div_0<-replace_na(kk_div_na,0)
  kk_mean<-round(mean(kk_div_0),3)
  MetricAverage<-c(MetricAverage,kk_mean)
}







