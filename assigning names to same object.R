library(tidyverse)
library(plyr)

assign(paste0("beta=",Figure2$beta," theta=",Figure2$theta,"metric=",
       Figure2$MetricAveragefinal,"randmod=",Figure2$randmod),Figure2)

rm(masterdf)
rm(df)
masterdf<-list(mget(ls()))%>%reduce(full_join,by="theta")
df<-ldply(masterdf,data.frame)
