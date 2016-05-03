library(dplyr)
library(tidyr)
library(ggplotheatmap)
library(ggplot2)

data(pms)


wide_to_tall <- function(data){
  data %>% add_rownames(var = "x") %>% mutate(x=as.integer(x)) %>% gather("y","value",-x)
}

tall_to_wide <- function(data){
  w %>% spread(y,value) %>% arrange(x) %>% select(-x)
}


StatHeat <- ggproto("StatHeat", Stat,required_aes = c("x","y"),
                    compute_panel = function(self,data, scales) {

                      # In here we need to split by GROUP and PANEL, do clustering and then rejoin after
                      
                      
                      # browser()
                      x <- data[,c('x','y','fill')] %>% spread(y,fill) %>% arrange(x) %>% select(-x)
                      row.hc <- hclust(dist(x))
                      col.hc <- hclust(dist(t(x)))

                      col.ord <- col.hc$order
                      row.ord <- row.hc$order
                      xx <- x[row.ord,col.ord]
                      xt <- wide_to_tall(xx) %>% mutate(x=as.numeric(x)) %>% mutate(y=as.integer(y))
                      nd <- xt %>% cbind(group=rep(1,nrow(.))) %>% cbind(PANEL=rep(1,nrow(.))) %>% rename(fill=value)
                      nd[,c('fill','x','y','PANEL','group')]
                    }
)

stat_heat <- function(mapping = NULL, data = NULL, geom = "tile",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  
  layer(
    stat = StatHeat, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


data <- wide_to_tall(as.data.frame(pms))
ggplot(data,aes(x=x,y=y)) + stat_heat(aes(fill=value))

#x <- as.data.frame(pms)
#ggplot(x) + stat_heat()
# 
# gh <- ggheat(as.data.frame(pms))  
# 
# gh + stat_heat()
# 


