
setClass("GGHeat",
         slots = list(data = "ANY",
                      main_plot = "ggORNULL", #Central ggplot where we show the heatmap itself
                      row_margin_plots = "list",
                      col_margin_plots = "list"),
         prototype = list(data = NULL))

# TODO: Implement the + method so that ggplot functions are passed through to the underlying ggplot object
setMethod("+", c("GGHeat"), function(e1, e2){})


##' Draw a heatmap from gene expression data
##'
##' 
##' @title ggheat
##' @param eset ExpressionSet, matrix or data frame containing gene expression data. Genes as rows and samples as columns
##' @param ... additional parameters passed to the underlying ggplot object
ggheat <- function(eset,...){
  #
  # TODO: Here goes code to create the ggplot object for the central view
  # 
  new("GGHeat",data=eset)
}

