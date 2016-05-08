


StatHeat <- ggproto("StatHeat", Stat,
                    
                    required_aes = c("x","y"),
                    
                    compute_panel = function(self,data, scales, cluster_aes) {
                      
                      
                      # browser()
                      # In here we need to split by GROUP and PANEL, do clustering and then rejoin after
                      #split_data <- split(data,list(data$group,data$PANEL))
                      
                      
                      # TODO: Need to deal with situation where fill is not provided
                      # Or where a different aes is used
                      #
                      original_columns <- names(data)
                      cluster_columns <- c('x','y',cluster_aes)
                      clusterable_data <- data[,cluster_columns]
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
                      
                      x <-  clusterable_data %>% spread_("y",cluster_aes) %>% arrange(x) %>% select(-x)
                      row.hc <- hclust(dist(x))
                      col.hc <- hclust(dist(t(x)))
                      
                      col.ord <- col.hc$order
                      row.ord <- row.hc$order
                      xx <- x[row.ord,col.ord]
                      xt <- wide_to_tall(xx) %>% mutate(x=as.numeric(x)) %>% mutate(y=as.integer(y))
                      
                      
                      
                      # nd <- xt %>% cbind(group=rep(1,nrow(.))) %>% cbind(PANEL=rep(1,nrow(.))) %>% rename_(cluster_aes="value")
                      # nd[,c('fill','x','y','PANEL','group')]
                      
                      nd <- cbind(non_clusterable_data,xt)
                      names(nd)[names(nd) == "value"] = cluster_aes
                      
                      nd[,original_columns]
                    }
)


#' Add a heatmap layer to a plot
#'
#' @export
stat_heat <- function(mapping = NULL, data = NULL, geom = "tile",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  
  # TODO: Check and provide an error if appropriate aes not set
  #
  cluster_aes = names(which(as.character(mapping)=="value"))[1]
  
  layer(
    stat = StatHeat, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cluster_aes = cluster_aes, ...)
  )
}
