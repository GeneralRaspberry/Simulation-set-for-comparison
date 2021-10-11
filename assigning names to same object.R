library(tidyverse)
library(plyr)

assign(paste0("beta=",Figure2$beta," theta=",Figure2$theta,"metric=",
       Figure2$MetricAveragefinal),Figure2)

masterdf<-list(mget(ls()))%>%reduce(full_join,by="theta")
df<-ldply(masterdf,data.frame)
