library(ggplot2)

#################load dataframe with all values named y################

########ensure all in correct order without duplicate####
y<-arrange(y,randmod,theta)
y<-distinct(y)


#############then take average metric and include#################
ymod<-y%>%
  group_by(randmod)%>%
  summarise_at(vars(MetricAveragefinal),list(name = mean))

y$metricplot<-unlist(ymod[rep(seq_len(nrow(ymod)),each=10),2])

#########now plot
ggplot(y,aes(x=as.factor(metricplot),y=theta,fill=reldif)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="Relative Difference")