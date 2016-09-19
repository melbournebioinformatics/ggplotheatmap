


#' Add a heatmap layer to a plot
#'
#' @export
stat_heat <- function(mapping = aes(fill=value), data = NULL, geom = "heat",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatHeat, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,... )
  )
}


#' @export
StatHeat <- ggproto("StatHeat", Stat,
                    required_aes = c("cluster_by"),
                    default_aes = aes(x= ..rowx.. ,y= ..coly.. ),
                    # 
                    # setup_params = function(data, params) {
                    #   params$allrowids <- data$rowid
                    #   params$allcolids <- data$colid
                    #   params
                    # },
                    compute_group = function(self,data, scales,cluster_aes = "cluster_by"){
                      
                      if(nrow(data) < 2){ return(data) }
                      
                      original_columns <- names(data)
                      
                      
                      cluster_columns <- c('rowid','colid',cluster_aes)
                      clusterable_data <- data[,cluster_columns]
                      
                      # This is kept because it might contain data for other aesthetics
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]
                      
                      # Convert from tall to wide and remove the x column so all we have
                      # are the matrix of clusterable values
                      
                      x <-  clusterable_data %>% spread_("colid",cluster_aes) 
                      
                      xrowids <- x$rowid
                      
                      x <- x %>% select(-rowid)
                      xcolids <- x %>% colnames()
                      
                      # Ensure that the matrix is entirely numeric
                      xnum <- apply(x,2,function(col) as.numeric(col))
                      
                      # Compute the clustering order
                      row.hc <- hclust(dist(xnum))
                      col.hc <- hclust(dist(t(xnum)))
                      
                      col.ord <- col.hc$order
                      row.ord <- row.hc$order

                      data$coly <- factor(data$colid,levels = xcolids[col.ord])
                      data$rowx <- factor(data$rowid,levels = xrowids[row.ord])
                      # browser()
                      data
                    }
)


