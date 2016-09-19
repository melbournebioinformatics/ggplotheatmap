
#' Select a subset of plot data
#'
#' @export
stat_select <- function(mapping = NULL, data = NULL, geom = "point",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, select_fun = function(x){x}, ...) {
  layer(
    stat = StatSelect, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, select_fun = select_fun, ... )
  )
}


#' @export
StatSelect <- ggproto("StatSelect", Stat,

                    compute_group = function(self,data, scales,select_fun){
                      select_fun(data)
                    }
)


