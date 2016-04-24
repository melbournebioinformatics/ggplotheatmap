##' Draw a heatmap from gene expression data
##'
##' 
##' @title ggheat
##' @param eset ExpressionSet, matrix or data frame containing gene expression data. Genes as rows and samples as columns
##' @param ... additional parameters passed to ggplot

ggheat.obj <- setClass("GGHeat",
                      slots = list(
                        data = "ANY",
                        main_plot = "ggORNULL", #Central ggplot where we show the heatmap itself
                        row_margin_plots = "list",
                        col_margin_plots = "list"),
                      prototype = list(data = NULL),
                      contains = "Cache")


ggheat <- function(eset,...){
  
}

ggheat <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggheat")
}

ggheat.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
  ggheat.data.frame(fortify(data, ...), mapping, environment = environment)
}

ggplot.data.frame <- function(data, mapping = aes(), ...,
                              environment = parent.frame()) {

  p <- structure(list(
    data = data,
    plot_env = environment
  ), class = c("ggheat","ggplot"))
  
  p
}

is.ggheat <- function(x) inherits(x, "ggheat")
