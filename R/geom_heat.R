#' Draw heatmaps.
#'
#' \code{geom_heat} creates a heatmap by combining \code{geom_tile} to produce the map itself and \code{geom_segment} to display a dendrogram based on the current clustering.
#'
#' @export
geom_heat <- function(mapping = aes(fill=value), data = NULL,
                      stat = "heat", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  
  # mapping <- add_clusterby_aes(mapping)
  # cluster_aes <- names(which(as.character(mapping)=="value"))[1]
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHeat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      # cluster_aes = cluster_aes,
      ...
    )
  )
}

#' @export
GeomHeat <- ggproto("GeomHeat", GeomRect,
                    extra_params = c("na.rm", "width", "height"),
                    
                    setup_data = function(data, params) {
                      
                      data$nx <- as.integer(data$x)
                      data$ny <- as.integer(data$y)
                      data$width <- data$width %||% params$width %||% resolution(data$nx, FALSE)
                      data$height <- data$height %||% params$height %||% resolution(data$ny, FALSE)

                      
                      # data$width <- data$width*0.5
                      
                      transform(data,
                                xmin = nx - width / 2,  xmax = nx + width / 2,  width = NULL,
                                ymin = ny - height / 2, ymax = ny + height / 2, height = NULL
                      )
                    },
                    
                    default_aes = aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                                      alpha = NA),
                    
                    required_aes = c("x", "y"),
                    
                    draw_key = draw_key_polygon
)
