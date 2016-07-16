
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

#' @export
StatHeat <- ggproto("StatHeat", Stat, required_aes = c("x","y"),
                    
                    compute_group = function(self,data, scales, cluster_aes) {
                      if(nrow(data) < 2){ return(data) }

                                            # browser()
                      original_columns <- names(data)
                      
                      # We know the x and y columns must be present as these are added when initialising the GGHeat object
                      # x is always the row number
                      # y is always the column number
                      #
                      cluster_columns <- c('x','y',cluster_aes)
                      clusterable_data <- data[,cluster_columns]
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
                      
                      # Convert from tall to wide and remove the x column so all we have
                      # are the matrix of clusterable values
                      x <-  clusterable_data %>% spread_("y",cluster_aes) %>% arrange(x) %>% select(-x)
                      
                      # Ensure that the matrix is entirely numeric
                      xnum <- apply(x,2,function(col) as.numeric(col))
                      
                      # Compute the clustering order
                      row.hc <- hclust(dist(xnum))
                      col.hc <- hclust(dist(t(xnum)))
                      
                      col.ord <- col.hc$order
                      row.ord <- row.hc$order
                      
                      # Make a new matrix with the clustering order
                      xx <- x[row.ord,col.ord]
                      
                      # Convert back to tall. This make a new x and new y based on the new ordering in the matrix
                      xx <- data.frame(xx,xord=row.ord,yord=col.ord)
                      colnames(xx) <- c(1:ncol(x),"xord","yord")
                      xt <- wide_to_tall(xx,id.vars=c("xord","yord")) %>% mutate(x=as.numeric(x)) %>% mutate(y=as.integer(y))
                      
                      
                      
                      # nd <- xt %>% cbind(group=rep(1,nrow(.))) %>% cbind(PANEL=rep(1,nrow(.))) %>% rename_(cluster_aes="value")
                      # nd[,c('fill','x','y','PANEL','group')]
                      
                      nd <- cbind(non_clusterable_data,xt)
                      names(nd)[names(nd) == "value"] = cluster_aes
                      nd <- nd[,c(original_columns,"xord","yord")]
                      data.frame(nd)
                      
                      # browser()
                      
                      # nd[,original_columns]
                      }
)


