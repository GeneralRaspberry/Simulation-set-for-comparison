#########initial set up################################

x<-Figure2
y<-rbind(Figure2,x)

##############binding onto existing dataframe##############
y<-rbind(y,Figure2)

##################rename if needed#########################

names(Figure2)[names(Figure2)=="mean_r"]<-"mean_rsave"