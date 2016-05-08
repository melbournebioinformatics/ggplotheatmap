library(dplyr)
library(tidyr)
library(ggplotheatmap)
library(ggplot2)

data(pms)


# data <- wide_to_tall(as.data.frame(pms))
# ggplot(data,aes(x=x,y=y)) + stat_heat(aes(fill=value))

data("squid")

c("grp","iBAQ","prot_id","protein_group")
sqd <- cbind(grp = rep(c(1,2),each=50),squid)

sqwd <- wide_to_tall(sqd,c("grp","iBAQ","prot_id","protein_group"))
ggplot(sqwd,aes(x=x,y=y,group=grp)) + stat_heat(aes(fill=value))

#x <- as.data.frame(pms)
#ggplot(x) + stat_heat()
# 
# gh <- ggheat(as.data.frame(pms))  
# 
# gh + stat_heat()
# 


