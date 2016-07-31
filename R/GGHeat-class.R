
aes.update <- function (aes, ...) 
{
  aes_new <- structure(as.list(match.call()[-c(1,2)]), class="uneval")
  aes_new <- ggplot2:::rename_aes(aes_new)
  aes[names(aes_new)] <- aes_new
}

setClass("GGHeat",slots = list(data = "ANY",main_plot = "gg", dendros = "list"),prototype = list(data = NULL,dendros = list()))

#' Create a ggheat object
#'
#' @param data heatmap data in wide format, possibly also including columns with contextual data
#' @return a ggheat object
#' @export
ggheat <- function(data,mapping = aes(group=1),
                   id.vars=c(),
                   distfun = dist,hclustfun= hclust,
                   rlabels=rownames(data),
                   clabels=colnames(data),
                   
                   cold=TRUE,rowd=TRUE,...){

  rownames(data) <- rlabels
  colnames(data) <- clabels
  melted_data <- wide_to_tall(data,id.vars)

  # browser()
  # By default there should be no grouping but in order to enforce this we must explicitly set the group
  # because the ggplot default is to group by all discrete variables present in the data
  # hmp <- ggplot(melted_data,aes(x=rowid,y=colid,rowid=rowid,colid=colid,cluster_by=value,group=1))
  default_aes <- aes(rowid=rowid,colid=colid,cluster_by=value)
  default_aes[names(mapping)] <- mapping
  
  
  hmp <- ggplot(melted_data,default_aes)
  
  new("GGHeat",data=data,main_plot=hmp)
}


setMethod("+", c(e1 = "GGHeat"), function(e1, e2){
  e1@main_plot <- e1@main_plot + e2
  # browser()
  e1
})

setMethod("show", "GGHeat", function(object){
  print(object@main_plot)
})
