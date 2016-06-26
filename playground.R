library(dplyr)
library(tidyr)
library(ggplotheatmap)
library(ggplot2)

data(squid)
sqg <- cbind(grp = rep(c(1,2),each=50),squid)

gh <- ggheat(sqg, id.vars = colnames(sqg[,1:4]))

ghs <- gh + stat_heat(aes(fill=value,group=grp))

gh + stat_heat(aes(fill=value)) + theme_ggheat()  + facet_wrap(~grp) +coord_flip() + dendro()
