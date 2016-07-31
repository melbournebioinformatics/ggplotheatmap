
# Return unique columns
# This is used for figuring out which columns are constant within a group
#
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop = FALSE]
  rownames(df) <- 1:nrow(df)
  df
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "")
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}


panel_scales <- function(panel, i) {
  this_panel <- panel$layout[panel$layout$PANEL == i, ]
  
  list(
    x = panel$x_scales[[this_panel$SCALE_X]],
    y = panel$y_scales[[this_panel$SCALE_Y]]
  )
}


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
#' 
#' #' @export
#' StatHeat <- ggproto("StatHeat", Stat, 
#'                     required_aes = c("cluster_by"),
#'                     default_aes = aes(x= ..rowx.. ,y= ..coly.. ),
#'                     # default_aes = aes(x=~rowid,y=~colid),
#'                     compute_layer = function(self, data, params, panels) {
#'                       
#'                       data <- remove_missing(data, params$na.rm,
#'                                              c(self$required_aes, self$non_missing_aes),
#'                                              snake_class(self),
#'                                              finite = TRUE
#'                       )
#'                       
#'                       # Trim off extra parameters
#'                       params <- params[intersect(names(params), self$parameters())]
#'                       
#'                       args <- c(list(data = quote(data), scales = quote(scales)), params)
#'                       plyr::ddply(data, "PANEL", function(data) {
#'                         scales <- panel_scales(panels, data$PANEL[1])
#'                         # browser()
#'                         tryCatch(do.call(self$compute_panel, args), error = function(e) {
#'                           warning("Computation failed in `", snake_class(self), "()`:\n",
#'                                   e$message, call. = FALSE)
#'                           data.frame()
#'                         })
#'                       })
#'                     },
#'                     
#'                     compute_group = function(self,data, scales, cluster_aes = "cluster_by") {
#'                       if(nrow(data) < 2){ return(data) }
#'                       # browser()
#'                       # data$rowx <- rep(1:3,times=3)
#'                       # data$coly <- rep(1:3,each=3)
#'                       # data$rowx <- rowid
#'                       # data$coly <- colid
#'                       # data$y <- rep(1:3,each=3)
#'                       data$coly <- factor(data$colid,levels=unique(data$colid))
#'                       data$rowx <- factor(data$rowid,levels=unique(data$rowid))
#'                                             
#'                       return(data)
#'                     }
#' )




#' @export
StatHeat <- ggproto("StatHeat", Stat,
                    required_aes = c("cluster_by"),
                    default_aes = aes(x= ..rowx.. ,y= ..coly.. ),
                    # 
                    setup_params = function(data, params) {
                      params$allrowids <- data$rowid
                      params$allcolids <- data$colid
                      params
                    },
                    #                     
                    #                     setup_data = function(data, params) {
                    #                       browser()
                    #                                             data
                    #                     },
                    
                    # compute_layer = function(self, data, params, panels) {
                    #   browser()
                    #   scales <- list(
                    #     x = factor(data$rowid),
                    #     y = factor(data$colid)
                    #   )
                    #   ggproto_parent(Stat,self)$compute_layer(data,params,panels)
                    # },
                    # 
                    # compute_panel = function(self, data, scales, ...) {
                    #   browser()
                    # 
                    #   ggproto_parent(Stat,self)$compute_panel(data,scales,...)
                    # },
                    
                    
# 
#                     compute_layer = function(self, data, params, panels) {
#                       # check_required_aesthetics(
#                       #   self$stat$required_aes,
#                       #   c(names(data), names(params)),
#                       #   snake_class(self$stat)
#                       # )
# 
#                       data <- remove_missing(data, params$na.rm,
#                                              c(self$required_aes, self$non_missing_aes),
#                                              snake_class(self),
#                                              finite = TRUE
#                       )
# 
#                       # Trim off extra parameters
#                       params <- params[intersect(names(params), self$parameters())]
# 
#                       args <- c(list(data = quote(data), scales = quote(scales)), params)
#                       ld <- plyr::ddply(data, "PANEL", function(data) {
#                         # browser()
#                         scales <- panel_scales(panels, data$PANEL[2])
#                         tryCatch(do.call(self$compute_panel, args), error = function(e) {
#                           warning("Computation failed in `", snake_class(self), "()`:\n",
#                                   e$message, call. = FALSE)
#                           data.frame()
#                         })
#                       })
#                       browser()
#                       ld
#                     },

                    
                    # compute_panel = function(self, data, scales, ...) {
                    #   if (empty(data)) return(data.frame())
                    #   
                    #   groups <- split(data, data$group)
                    #   stats <- lapply(groups, function(group) {
                    #     self$compute_group(data = group, scales = scales, ...)
                    #   })
                    #   
                    #   browser()
                    #   
                    #   stats <- mapply(function(new, old) {
                    #     if (empty(new)) return(data.frame())
                    #     unique <- uniquecols(old)
                    #     missing <- !(names(unique) %in% names(new))
                    #     cbind(
                    #       new,
                    #       unique[rep(1, nrow(new)), missing,drop = FALSE]
                    #     )
                    #   }, stats, groups, SIMPLIFY = FALSE)
                    #   
                    #   do.call(plyr::rbind.fill, stats)
                    # },
                    # 
                    
                    compute_group = function(self,data, scales, cluster_aes = "cluster_by", allrowids, allcolids) {
                      if(nrow(data) < 2){ return(data) }
                      
                      original_columns <- names(data)
                      # browser()
                      
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
                      
                      # xxx <- xnum[row.ord,col.ord]
                      
                      data$coly <- factor(data$colid,levels = xcolids[col.ord])
                      data$rowx <- factor(data$rowid,levels = xrowids[row.ord])
                      
                      data
                    }
)


