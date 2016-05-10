
setClass("GGHeat",slots = list(data = "ANY",main_plot = "gg"),prototype = list(data = NULL))

#' Create a ggheat object
#'
#' @param data heatmap data in wide format, possibly also including columns with contextual data
#' @return a ggheat object
#' @export
ggheat <- function(data,id.vars=c(),
                   distfun = dist,hclustfun= hclust,
                   rlabels=rownames(data),
                   clabels=colnames(data),
                   cold=TRUE,rowd=TRUE,...){

  
  melted_data <- wide_to_tall(data,id.vars)

  # By default there should be no grouping but in order to enforce this we must explicitly set the group
  # because the ggplot default is to group by all discrete variables present in the data
  hmp <- ggplot(melted_data,aes(x=x,y=y,group=1))

  new("GGHeat",data=data,main_plot=hmp)
}


setMethod("+", c("GGHeat"), function(e1, e2){
  e1@main_plot <- e1@main_plot + e2
  e1
})

setMethod("show", "GGHeat", function(object){
  print(object@main_plot)
})
