library(tidyr)#for the replace NA

w<-owin(xrange=c(0,100),yrange=c(0,100))

pp<-ppp(x=data$x,y=data$y, window = w)

data %>% group_by(sim) %>%
  do(data.frame(time=times, infected=sapply(times, function(x) sum(.$time <= x))))
}
datasplice<-data%>%top_n(1000)
isolatemetric<-function(x,y){
  pp<-ppp(x=x,y=y, window = w)
  kkk<-Kest(pp)
  #kkk_division<-kkk$iso/kkk$theo
  #Kkk_division_0<-replace_na(kkk_division,0)
  #Kkk_mean<-round(mean(Kkk_division,0))
}

datatable<-data %>% group_by(sim) %>%
  do(data.frame(sim=sim, Metric=mapply(isolatemetric,x=datasplice$x,y=datasplice$y)))


mapply(isolatemetric,x=datasplice$x,y=datasplice$y)
datasplice
