library(ggplot2)

ggplot(y,aes(x=(MetricAveragefinal),y=theta,fill=reldif)) +
  geom_tile() + 
  scale_fill_gradient(low="white",
                      high="darkred",
                      name="Relative Difference")