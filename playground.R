library(dplyr)
library(tidyr)
library(ggplotheatmap)
library(ggplot2)

data(squid)
sqg <- cbind(grp = rep(c(1,2),each=50),squid)

gh <- ggheat(sqg, id.vars = colnames(sqg[,1:4]))

gh + stat_heat(aes(fill=value,group=grp),geom="heat")

gh + geom_heat(aes(fill=value,group=grp))

gh + stat_heat(aes(fill=value),geom="heat") + theme_ggheat()  + facet_wrap(~grp) +coord_flip()


gh + stat_heat(aes(fill=value),geom="heat") + facet_wrap(~grp) +  stat_clust(aes(color=value)) +  stat_clust(aes(color=value),cluster_by = "y", relsize=0.05) 


gh + stat_heat(aes(fill=value),geom="heat") + facet_wrap(~grp) +  stat_clust() 

#+ coord_flip()

heatmap_components(sqg[,5:9])
