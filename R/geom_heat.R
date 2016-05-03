



StatHeat <- ggproto("StatHeat", Stat,
                    required_aes = c("x","y"),
                    compute_group = function(data, scales) {
                      data
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


