

#' cluster_axis must be x or y indicates which axis to cluster by
#' Add a heatmap layer to a plot
#'
#' @export
stat_clust <- function(mapping = NULL, data = NULL, 
                       geom = "segment",position = "identity", ...,
                       na.rm = FALSE, 
                       show.legend = NA,
                       inherit.aes = TRUE,
                       cluster_axis = "x",
                       relsize = 0.2) {

  # mapping <- add_clusterby_aes(mapping)
  # cluster_aes <- names(which(as.character(mapping)=="value"))[1]
  
  layer(
    stat = StatClust, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      # cluster_aes = cluster_aes,
      cluster_axis = cluster_axis,
      relsize = relsize,
      ...
    )
  )
}

# Rescale the range of y so that yscale is a set proportion of the original and offset by ys
rescale_dendro_y <- function(dd,ys,yscale){
  ymin = min(dd$yend,dd$y)
  ymax = max(dd$yend,dd$y)
  range=ymax-ymin
  dd$y <- (dd$y-ymin)*yscale/range  + ys
  dd$yend <- (dd$yend-ymin)*yscale/range + ys
  dd
}

# Rescale the range of x so that xscale is a set proportion of the original and offset by ys
rescale_dendro_x <- function(dd,xs,xscale){
  xmin = min(dd$xend,dd$x)
  xmax = max(dd$xend,dd$x)
  range=xmax-xmin
  dd$x <- (dd$x-xmin)*xscale/range  + xs
  dd$xend <- (dd$xend-xmin)*xscale/range + xs
  dd
}



#' @export
StatClust <- ggproto("StatClust", Stat, 
                     required_aes = c("cluster_by"),

                     setup_params = function(data, params) {
                       # browser()
                       params$row_group_widths <- sapply(split(data,data$group),function(x){nrow(x)})/length(unique(data$colid))
                       params
                     },
                     
                    compute_group = function(self,data, scales, cluster_aes = "cluster_by", cluster_axis = "x", relsize = 0.2, row_group_widths) {
                      if(nrow(data) < 2){ return(data) }
                      
                      original_columns <- names(data)
                      
                      # We know the x and y columns must be present as these are added when initialising the GGHeat object
                      # x is always the row number
                      # y is always the column number
                      #
                      cluster_columns <- c('rowid','colid',cluster_aes)
                      clusterable_data <- data[,cluster_columns]
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
                      
                      # Convert from tall to wide and remove the x column so all we have
                      # are the matrix of clusterable values
                      # xrowids <- clusterable_data %>% spread_("y",cluster_aes) %>% arrange(x) 
                      # x <-  clusterable_data %>% spread_("y",cluster_aes) %>% arrange(x) %>% select(-x)

                      
                      x <-  clusterable_data %>% spread_("colid",cluster_aes) 
                      
                      xrowids <- x$rowid
                      
                      x <- x %>% select(-rowid)
                      xcolids <- x %>% colnames()
                      
                      # Ensure that the matrix is entirely numeric
                      xnum <- apply(x,2,function(col) as.numeric(col))
                      
                      # Compute the clustering order
                      row.hc <- hclust(dist(xnum))
                      col.hc <- hclust(dist(t(xnum)))
                      
                      # 
                      # # Ensure that the matrix is entirely numeric
                      # xnum <- apply(x,2,function(col) as.numeric(col))
                      # 
                      # # Compute the clustering order
                      # row.hc <- hclust(dist(xnum))
                      # col.hc <- hclust(dist(t(xnum)))

                      row.dendro <- dendro_data(as.dendrogram(row.hc),type="rectangle")
                      col.dendro <- dendro_data(as.dendrogram(col.hc),type="rectangle")
                      
                      rsd <- NULL
                      if ( cluster_axis == "x" ){
                        rowd <- segment(row.dendro)
                        # browser()
                        gn <- data$group[1]
                        
                        # When we cluster by x we use y to set the relsize
                        rsd <- rescale_dendro_y(rowd,ncol(x)+0.5,ncol(x)*relsize)
                        if ( gn > 1 ){
                          x_offset <- sum(row_group_widths[1:(gn-1)])
                          rsd <- rescale_dendro_x(rsd,x_offset+1,(nrow(x)-1))
                        }
                      } else if ( cluster_axis == "y"){
                        browser()
                        cold <- segment(col.dendro)
                        cold <- data.frame(x=cold$y,y=cold$x,yend=cold$xend,xend=cold$yend)
                        
                        xvals <- as.numeric(rownames(x))
                        rsd <- rescale_dendro_x(cold,max(xvals)+0.5,max(xvals)*relsize)                        
                      } else {
                        stop("Invalid value for cluster_axis. Must be set to x or y ",cluster_axis)
                      }
                      rsd
                    }
                  )


