data(pms)
gh <- ggheat(as.data.frame(pms))  

gh + stat_heat()
