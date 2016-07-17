
#' Add a heatmap layer to a plot
#'
#' @export
stat_heat <- function(mapping = NULL, data = NULL, geom = "tile",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatHeat, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @export
StatHeat <- ggproto("StatHeat", Stat, 
                    required_aes = c("cluster_by"),
                    default_aes = aes(x=..rowx..,y=..coly..),
                    compute_group = function(self,data, scales, cluster_aes = "cluster_by") {
                      if(nrow(data) < 2){ return(data) }
  
                      original_columns <- names(data)
                      
                      # browser()
                      # We know the x and y columns must be present as these are added when initialising the GGHeat object
                      # x is always the row number
                      # y is always the column number
                      #
                      cluster_columns <- c('rowid','colid',cluster_aes)
                      clusterable_data <- data[,cluster_columns]
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
                      
                      # Convert from tall to wide and remove the x column so all we have
                      # are the matrix of clusterable values
                      xrowids <- clusterable_data %>% spread_("colid",cluster_aes) %>% arrange(rowid) 
                      x <-  clusterable_data %>% spread_("colid",cluster_aes) %>% arrange(rowid) %>% select(-rowid)
                      
                      # Ensure that the matrix is entirely numeric
                      xnum <- apply(x,2,function(col) as.numeric(col))
                      
                      # Compute the clustering order
                      row.hc <- hclust(dist(xnum))
                      col.hc <- hclust(dist(t(xnum)))
                      
                      col.ord <- col.hc$order
                      row.ord <- row.hc$order
                      
                      #
                      # Make a new matrix with the clustering order. 
                      # This matrix allows us to calculate the derived variables rowx and coly which are the numeric positions 
                      # corresponding to the properly clustered matrix
                      # We need to retain the original row and columns ids though for labelling and for other plots
                      #
                      xx <- x[row.ord,col.ord]

                      # browser()
                      # Convert back to tall. This makes a new x and new y based on the new ordering in the matrix
                      xx <- data.frame(xx,rowx=1:nrow(xx),rowid=xrowids$rowid[row.ord])
                      colnames(xx) <- c(1:ncol(x),"rowx","rowid")
                      
                      
                      xt <- xx %>% mutate(rowid=as.integer(rowid)) %>% gather_("colid","value",setdiff(colnames(xx),c("rowx","coly","rowid","colid")))
                      xt$coly <- as.numeric(xt$colid)

                      
                      
                      # nd <- xt %>% cbind(group=rep(1,nrow(.))) %>% cbind(PANEL=rep(1,nrow(.))) %>% rename_(cluster_aes="value")
                      # nd[,c('fill','x','y','PANEL','group')]
                      
                      nd <- cbind(non_clusterable_data,xt)
                      names(nd)[names(nd) == "value"] = cluster_aes
                      nd <- nd[,c(original_columns,"rowx","coly")]
                      data.frame(nd)
                      
                      # browser()
                      
                      # nd[,original_columns]
                      }
)


