
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
                    # default_aes = aes(x=~rowid,y=~colid),


                    compute_group = function(self,data, scales, cluster_aes = "cluster_by") {
                      if(nrow(data) < 2){ return(data) }

                      original_columns <- names(data)
                      # browser()
                      
                      cluster_columns <- c('rowid','colid',cluster_aes)
                      clusterable_data <- data[,cluster_columns]

                      # This is kept because it might contain data for other aesthetics
                      non_clusterable_data <- data[,setdiff(original_columns,cluster_columns)]

                      # Convert from tall to wide and remove the x column so all we have
                      # are the matrix of clusterable values
                      # xrowids <- clusterable_data %>% spread_("colid",cluster_aes) %>% arrange(rowid)
                      
                      x <-  clusterable_data %>% spread_("colid",cluster_aes) 
                      
                      xrowids <- x$rowid
                      
                      x <- x %>% select(-rowid)
                      xcolids <- x %>% colnames()
                      #%>% arrange(rowid) %>% select(-rowid)

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
                                            
                      # browser()
                      # # scales$x <- scale_x_discrete(labels = as.character(row.ord))$get_limits()
                      # #
                      # # Make a new matrix with the clustering order.
                      # # This matrix allows us to calculate the derived variables rowx and coly which are the numeric positions
                      # # corresponding to the properly clustered matrix
                      # # We need to retain the original row and columns ids though for labelling and for other plots
                      # #
                      # xx <- x[row.ord,col.ord]
                      # 
                      # 
                      # # browser()
                      # # Convert back to tall. This makes a new x and new y based on the new ordering in the matrix
                      # xx <- data.frame(xx,rowid=as.character(xrowids$rowid[row.ord]),stringsAsFactors = FALSE)
                      # # colnames(xx) <- c(col.ord,"rowid")
                      # 
                      # 
                      # xt <- xx %>% gather_("colid","value",setdiff(colnames(xx),c("x","y","rowid","colid")))
                      # xt$rowid <- as.character(xt$rowid)
                      # # browser()
                      # 
                      # xt$coly <- xt$colid
                      # xt$rowx <- xt$rowid
                      # 
                      # # This is essential in order to rejoin with the original data
                      # xt <- xt %>% arrange(colid,rowid)
                      # 
                      # 
                      # # nd <- xt %>% cbind(group=rep(1,nrow(.))) %>% cbind(PANEL=rep(1,nrow(.))) %>% rename_(cluster_aes="value")
                      # # nd[,c('fill','x','y','PANEL','group')]
                      # 
                      # nd <- cbind(non_clusterable_data,xt)
                      # names(nd)[names(nd) == "value"] = cluster_aes
                      # nd <- nd[,c(original_columns)]
                      # 
                      # # browser()
                      # nd$coly <- factor(nd$coly,levels=colnames(x[,col.ord]))
                      #                       data.frame(nd)


                      # nd[,original_columns]
                      data
                      }
)


