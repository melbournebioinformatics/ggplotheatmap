setClass("GGHeatDendro",slots = list(type = "character"),prototype = list(type = 'row'))

#' Add a dendrogram to a plot
#'
#' @export
dendro <- function(type='row'){
  new("GGHeatDendro",type = type)
}

is_dendro <- function(object){
  is ( object, "GGHeatDendro")
}