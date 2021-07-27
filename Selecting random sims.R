################using colour brewer and selecting random sims#########################################################################

myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))


Rdmsim<- sample(1:length(unique(data$sim)),6,replace=FALSE)
dataframeforplot<-data%>%filter(sim %in% Rdmsim)

ggplot(dataframeforplot)+geom_point(aes(x=x,y=y,colour=time))+facet_grid(vars(sim))+
  ggtitle(paste0("Time until infection for ",diseasename,"\n \u03b2 = ", beta, " \u03b8 = ", theta, " LRF = ", randmod, " r = ", round(mean_r,4)))+
  theme_tufte()+
  scale_color_gradientn(colours = (myPalette(1000)))

