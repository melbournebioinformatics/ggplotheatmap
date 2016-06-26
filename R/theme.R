##' ggheat default theme
##'
##' 
##' @title theme_ggheat
##' @export
theme_ggheat <- function(){
  list(scale_x_discrete(expand = c(0,0)),
       scale_y_discrete(expand = c(0,0)),
    theme_ggheat_internal())
}

theme_ggheat_internal <- function(bgcolor="white", fgcolor="black", ...) {
  theme_bw() %+replace%
    theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
          ...)
}

