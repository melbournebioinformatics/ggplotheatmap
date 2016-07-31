library(dplyr)
library(tidyr)
library(ggplotheatmap)
library(ggplot2)

data(squid)
sqg <- cbind(grp = c(rep(1,length=10),rep(2,length=70),rep(3,length=20)),squid)

sqg
sd <- sqg[1:10,]
rownames(sd) <- c("a","c","b","d","e","f","g","h","i","j")
sd["b","DM"] <- 25599000000

pd <- matrix(1:9,nrow = 3)
colnames(pd) <- c("a","c","b")
rownames(pd) <- c("d","f","e")
pd <- data.frame(pd)

gh <- ggheat(pd) #, id.vars = colnames(sqg[,1:4]))
gh + stat_heat(aes(size=value),geom="point")

gh <- ggheat(sqg, id.vars = colnames(sqg[,1:4]),aes(group=grp))
gh + stat_heat(aes(fill=value),geom="heat") + facet_wrap(~grp, scales = "free")

gh + stat_heat(aes(fill=value),geom="heat") + coord_flip() 

gh <- ggheat(sqg, id.vars = colnames(sqg[,1:4]))
gh + stat_heat(aes(fill=value),geom="heat")  + stat_clust(aes(color=value),relsize=0.05)

cgh + stat_heat(aes(fill=value,group=grp),geom="heat") + coord_flip()

gh + geom_heat(aes(fill=value,group=grp))

gh + stat_heat(aes(fill=value),geom="heat") + theme_ggheat()  + facet_wrap(~grp) +coord_flip()


gh + stat_heat(aes(fill=value),geom="heat") + facet_wrap(~grp) +  stat_clust(aes(color=value)) +  stat_clust(aes(color=value),cluster_by = "y", relsize=0.05) 


gh + stat_heat(aes(fill=value),geom="heat") + facet_wrap(~grp) +  geom_point(aes(color=iBAQ),stat="heat")

#+ coord_flip()

heatmap_components(sqg[,5:9])
