enginefordispersal<-function(beta,b,theta,distance){
  ((beta*b)/(2*pi*theta*theta*gamma(2/b)))*exp(-distance^b/theta^b)
}

beta<-2000
theta<-25
b<-.5
distance<-seq(from=0, to=500,by=1)

for(y in distance){
  disperse<-enginefordispersal(beta,b,theta,distance)
}

dataframeb.4<-data.frame(disperse,distance,b,theta,beta)

b<-1
for(y in distance){
  disperse<-enginefordispersal(beta,b,theta,distance)
}

dataframe1<-data.frame(disperse,distance,b,theta,beta)

masterdf<-rbind(dataframe1,dataframeb.4)

library(tidyverse)

b<-.5
mdd.5<-theta*gamma(3/b)/gamma(2/b)

b<-1
mdd1<-theta*gamma(3/b)/gamma(2/b)


Freshtitle<-paste0("\u03b8 = ", theta, " \u03b2 =", beta)

ggplot(masterdf,aes(x=distance,
                    y=disperse,
                    group=b,
                    colour=as.factor(b)))+
  geom_line()+
  geom_vline(xintercept=mdd1, linetype="dashed", color="blue", size=.5)+
  geom_vline(xintercept=mdd.5, linetype="dashed", color="red", size=.5)+
  xlab("Distance (m)")+ylab("P(dispersal)")+labs(color="Shape Factor")+
  ggtitle(Freshtitle)
  

plot1<-ggplot(masterdf,aes(x=distance,
                    y=disperse,
                    group=b,
                    colour=as.factor(b)))+
  geom_line()+
  geom_vline(xintercept=mdd1, linetype="dashed", color="blue", size=.5)+
  geom_vline(xintercept=mdd.5, linetype="dashed", color="red", size=.5)+
  xlab("Distance (m)")+ylab("P(dispersal)")+labs(color="Shape Factor")+
  ggtitle(Freshtitle)


ggsave(filename = "Dispersal comparison.png",plot = plot1, 
       width = 30,
       height = 30,
       units = "cm")
  
