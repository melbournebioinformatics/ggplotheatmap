
# p <- ggheat(data[,sample_cols],cluster="",dist="") + geom_point(aes(y=iBAQ,x="Arms")) %>% 
#   add_genecontext() %>% 
#   add_samplecontext()

setClass("GGHeat",slots = list(data = "ANY",main_plot = "ggORNULL"),prototype = list(data = NULL))


melt_data <- function(data){
  x <- as.data.frame(data)
  x <- cbind(rownames(x),x)
  dd <- melt(id.vars=1,x)
  names(dd) <-c("x","y","value")
  dd$value <- as.numeric(dd$value)
  dd
}


ggheat <- function(data,distfun = dist,hclustfun= hclust,
                   rlabels=rownames(data),
                   clabels=colnames(data),
                   cold=TRUE,rowd=TRUE,...){

  
  melted_data <- melt_data(data)

  hmp <- ggplot(melted_data,aes(x=x,y=y))
  # browser()
  # hmp <- hmp + geom_tile(aes(fill=Intensity)) + scale_fill_gradient2(na.value="white",low="red",mid="grey90",high="blue") 
  
  
  new("GGHeat",data=data,main_plot=hmp)
}


# TODO: Implement the + method so that ggplot functions are passed through to the underlying ggplot object
setMethod("+", c("GGHeat"), function(e1, e2){
  e1@main_plot <- e1@main_plot + e2
  e1
})

setMethod("show", "GGHeat", function(object){
  print(object@main_plot)
})

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
  names(xxm) <- c("x","y","Intensity")
    # Ensure that our row and column orderings are respected by ggplot
  # xxm$rlabels <- factor(xxm$rlabels,levels=xx$rlabels)
  # xxm$clabels <- factor(xxm$clabels,levels=colnames(xx))
  xxm
}