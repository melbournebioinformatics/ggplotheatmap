
# 
# cluster_group <- function(data,cluster_aes){
#   original_columns <- names(data)
#   cluster_columns <- c('x','y',cluster_aes)
#   clusterable_data <- data[,cluster_columns]
#   non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
#   
#   x <-  clusterable_data %>% spread_("y",cluster_aes) %>% arrange(x) %>% select(-x)
#   row.hc <- hclust(dist(x))
#   col.hc <- hclust(dist(t(x)))
#   
#   col.ord <- col.hc$order
#   row.ord <- row.hc$order
#   xx <- x[row.ord,col.ord]
#   xt <- wide_to_tall(xx) %>% mutate(x=as.numeric(x)) %>% mutate(y=as.integer(y))
#   
#   
#   
#   # nd <- xt %>% cbind(group=rep(1,nrow(.))) %>% cbind(PANEL=rep(1,nrow(.))) %>% rename_(cluster_aes="value")
#   # nd[,c('fill','x','y','PANEL','group')]
#   
#   nd <- cbind(non_clusterable_data,xt)
#   names(nd)[names(nd) == "value"] = cluster_aes
#   
#   nd[,original_columns]  
# }

# StatHeat <- ggproto("StatHeat", Stat, required_aes = c("x","y"),
#                     
#                     compute_panel = function(self,data, scales, cluster_aes) {
#                       if (empty(data)) return(data.frame())                      
#                       groups <- split(data, data$group)
#                       stats <- lapply(groups, function(group) {
#                         self$cluster_group(data = group, cluster_aes)
#                       })                     
#                       
#                       stats <- mapply(function(new, old) {
#                         if (empty(new)) return(data.frame())
#                         unique <- uniquecols(old)
#                         missing <- !(names(unique) %in% names(new))
#                         cbind(
#                           new,
#                           unique[rep(1, nrow(new)), missing,drop = FALSE]
#                         )
#                       }, stats, groups, SIMPLIFY = FALSE)
#                       
#                       do.call(plyr::rbind.fill, stats)
#                     }
# )


StatHeat <- ggproto("StatHeat", Stat, required_aes = c("x","y"),
                    
                    compute_group = function(self,data, scales, cluster_aes) {
                      if(nrow(data) < 2){ return(data) }

                                            # browser()
                      original_columns <- names(data)
                      cluster_columns <- c('x','y',cluster_aes)
                      clusterable_data <- data[,cluster_columns]
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
                      
                      x <-  clusterable_data %>% spread_("y",cluster_aes) %>% arrange(x) %>% select(-x)
                      
                      xnum <- apply(x,2,function(col) as.numeric(col))
                      row.hc <- hclust(dist(xnum))
                      col.hc <- hclust(dist(t(xnum)))
                      
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
