
cluster_and_melt <- function(data,                   
                             distfun = stats::dist,
                             hclustfun= stats::hclust,
                             rlabels=rownames(data),
                             clabels=colnames(data),
                             cold=TRUE,rowd=TRUE,...){
  x <- data
  
  if ( !is.data.frame(x) ){ x <- as.data.frame(x)}
  
  if ( is.null(rlabels) ) { rlabels <- 1:nrow(x) }
  if ( is.null(clabels) ) { clabels <- 1:ncol(x) }  
  
  row.hc <- hclust(dist(x))
  col.hc <- hclust(dist(t(x)))
  
  row.dendro <- dendro_data(as.dendrogram(row.hc),type="rectangle")
  col.dendro <- dendro_data(as.dendrogram(col.hc),type="rectangle")
  
  ## dendro plots
  # col.plot <- ggplot(rescale_dendro(segment(col.dendro))) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) 
  # row.plot <- ggplot(rescale_dendro(segment(row.dendro))) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
  
  ## Scales need expanding so that dendros match central grid
  #
  # col.plot <- col.plot + scale_x_continuous(expand=c(0.5/ncol(x),0))
  # row.plot <- row.plot + scale_x_continuous(expand=c(0.5/nrow(x),0)) + scale_y_continuous(expand = c(0.01,0))
  
  ## Get dendro order and rearrange expression matrix accordingly
  if ( cold ){
    col.ord <- col.hc$order
  } else {
    col.ord <- 1:ncol(x)
    # col.plot <- NULL
  }
  
  if ( rowd ){
    row.ord <- row.hc$order
    # row.plot <- row.plot +  coord_flip() 
  } else {
    row.ord <- 1:nrow(x)
    # row.plot <- NULL
  }
  
  xx <- x[row.ord,col.ord]
  rlabels <- rlabels[row.ord]
  xx <- data.frame(rlabels,xx)
  xxm_idvars <- c('rlabels')
  
  xxm <- melt(xx,value.name = 'Intensity',id.vars = xxm_idvars,variable.name = 'clabels')
  
  # Ensure that our row and column orderings are respected by ggplot
  # xxm$rlabels <- factor(xxm$rlabels,levels=xx$rlabels)
  # xxm$clabels <- factor(xxm$clabels,levels=colnames(xx))
  xxm
}


StatHeat <- ggproto("StatHeat", Stat,
  compute_group = function(data, scales) {

      cluster_and_melt(data)

      },
                    
  required_aes = c(),
  default_aes = aes(x=..clabels..,y=..rlabels..)
)

stat_heat <- function(mapping = NULL, data = NULL, geom = "tile",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatHeat, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


