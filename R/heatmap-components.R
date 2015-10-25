#' Create grid and dendro parts of a heatmap as separate ggplot objects
#'
#' @param x matrix of intensity values
#' @param rlabels a character vector of row labels with length equal to number of rows in x
#' @param clabels a character vector of column labels with length equal to number of columns in x
#' @param cold whether to cluster columns and produce a column dendro
#' @param rowd whether to cluster rows and produce a row dendro
#' @return a named list with the following objects
#' \itemize{
#'  \item tile tiled intensity values
#'  \item rowd and cold row and column dendrograms
#' }\item transformed data used to plot tile, rowd and cold
#' @export
#' @examples
#' data(pms)
#'
#' hmc <- heatmap_components(x = pms)
#'
#' show_ggheatmap(hmc$tile,row_dendro = hmc$rowd,col_dendro = hmc$cold)

heatmap_components <- function(x,rlabels=rownames(x),clabels=colnames(x),cold=TRUE,rowd=TRUE,stripdata=NULL){
  
  if ( !is.data.frame(x) ){ x <- as.data.frame(x)}
  
  if ( is.null(rlabels) ) { rlabels <- 1:nrow(x) }
  if ( is.null(clabels) ) { clabels <- 1:ncol(x) }  
  
  colnames(x) <- clabels
  row.hc <- hclust(dist(x))
  col.hc <- hclust(dist(t(x)))
  
  row.dendro <- dendro_data(as.dendrogram(row.hc),type="rectangle")
  col.dendro <- dendro_data(as.dendrogram(col.hc),type="rectangle")
  
  ## dendro plots
  col.plot <- ggplot(rescale_dendro(segment(col.dendro))) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) 
  row.plot <- ggplot(rescale_dendro(segment(row.dendro))) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
  
  ## Scales need expanding so that dendros match central grid
  #
  col.plot <- col.plot + scale_x_continuous(expand=c(0.5/ncol(x),0))
  row.plot <- row.plot + scale_x_continuous(expand=c(0.5/nrow(x),0)) + scale_y_continuous(expand = c(0.01,0))
  
  ## Get dendro order and rearrange expression matrix accordingly
  if ( cold ){
    col.ord <- col.hc$order
  } else {
    col.ord <- 1:ncol(x)
    col.plot <- NULL
  }
  
  if ( rowd ){
    row.ord <- row.hc$order
    row.plot <- row.plot +  coord_flip() 
  } else {
    row.ord <- 1:nrow(x)
    row.plot <- NULL
  }
  
  xx <- x[row.ord,col.ord]
  rlabels <- rlabels[row.ord]
  xx <- data.frame(rlabels,xx)
  xxm_idvars <- c('rlabels')
  
  ## Include stripdata in tile data if available
  if ( !is.null(stripdata) ){
    if ( !is.data.frame(stripdata) ){
      stripdata <- as.data.frame(stripdata)
    }

    rownames(stripdata)<-rownames(x)
    stripdata <- stripdata[row.ord,,drop=FALSE]
    xx <- cbind(stripdata,xx)
    xxm_idvars <- setdiff(colnames(xx),colnames(x))
  }
  
  
  xxm <- melt(xx,value.name = 'Intensity',id.vars = xxm_idvars,variable.name = 'clabels')

  # Ensure that our row and column orderings are respected by ggplot
  xxm$rlabels <- factor(xxm$rlabels,levels=xx$rlabels)
  xxm$clabels <- factor(xxm$clabels,levels=colnames(xx))
  
  
  hmp <- ggplot(xxm,aes(clabels,rlabels)) 
  hmp <- hmp + geom_tile(aes(fill=Intensity)) + scale_fill_gradient2(na.value="white",low="red",mid="grey90",high="blue") 
  
  return(list(tile=hmp,rowd=row.plot,cold=col.plot,data=list(tile=xxm,row.ord=row.ord,col.ord=col.ord)))
  
}


rescale_dendro <- function(dd){
  xmin = min(dd$xend,dd$x)
  xmax = max(dd$xend,dd$x)
  range=xmax-xmin
  dd$x <- (dd$x-xmin)*1/range
  dd$xend <- (dd$xend-xmin)*1/range
  
  dd
}



