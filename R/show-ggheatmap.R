#' Displays ggplot based heatmap composed of multiple ggplot objects
#'
#' @param tile ggplot object with a tiled grid of intensity values as produced by heatmap_components
#' @param row_dendro row dendrogram as produced by heatmap_components
#' @param col_dendro col dendromgram as produced by heatmap_components
#' @param lstrip list containing ggplots to display as strips along left axis
#' @param bstrip list containing ggplots to display as strips along bottom axis
#' @param cold.height relative height of column dendro
#' @param rowd.width relative width of row dendro
#' @param lstrip.width relative width of lstrip plots
#' @param bstrip.height relative height of bstrip plots
#' @param laxis.width relative width of left axis
#' @param baxis.height relative height of bottom axis
#' @param legend.height relative height of legend
#' @param preserve_scales Allows you to use a custom x scale. If TRUE the x scale will not be replaced. Use expand(c(0.0,0)) on your custom scale.
#' @return a gtable object that can be displayed with grid.draw
#' @export
#' @examples
#'data(pms)

#'hmc <- heatmap_components(x = pms)

#'show_ggheatmap(hmc$tile,row_dendro = hmc$rowd,col_dendro = hmc$cold)
show_ggheatmap<-function(tile,row_dendro=NULL,col_dendro=NULL,lstrip=NULL,bstrip=NULL,
                         cold.height=0.2,rowd.width=0.2,lstrip.width=0.05,bstrip.height=0.05,
                         laxis.width=0.2,baxis.height=0.1,
                         legend.height=0.1,preserve_scales=FALSE)
{ 
  
  col_dendro <- col_dendro + theme_dendro()
  row_dendro <- row_dendro + theme_dendro()
  
  if (!preserve_scales){
    tile <- tile + scale_x_discrete(expand=c(0.0, 0)) 
  }
  
  tile <- tile + theme(legend.position="bottom")
  
  legends <- list()
  legends <- append(legends,list(extract.legend(tile)))
  legends <- append(legends,lapply(lstrip,extract.legend))
  legends <- append(legends,lapply(bstrip,extract.legend))
  
  n_rows <- length(bstrip) + 2 + as.integer(!is.null(col_dendro)) + length(legends)
  n_cols <- length(lstrip) + 2 + as.integer(!is.null(row_dendro))
  
  pad_top = as.integer(!is.null(col_dendro))
  pad_bottom <- n_rows - 1 - pad_top
  
  
  hm_parts <- list()
  
  # Build up the plot column by column
  #
  
  # First column is the left axis only
  #
  
  laxis_grob <- gtable_filter(ggplotGrob(tile),'axis-l')
  hm_parts <- append.col.for.grob(laxis_grob,hm_parts,pad_top,pad_bottom)
  
  
  # Next come the lstrip columns
  #
  for (strip in lstrip) {
    hm_parts <- append.col.for.grob(extract.panel(strip),hm_parts,pad_top,pad_bottom)
  }
  
  #
  # The central column
  #
  if ( !is.null(col_dendro) ){
    hm_parts <- append(hm_parts,list(extract.panel(col_dendro)))
  } else {
    cold.height=numeric(0)
  }
  
  # Central panel
  hm_parts <- append(hm_parts,list(extract.panel(tile)))
  
  # Bottom strips
  for (strip in bstrip) {
    hm_parts <- append(hm_parts,list(extract.panel(strip)))
  }
  
  # Bottom axis
  baxis_grob <- gtable_filter(ggplotGrob(tile),'axis-b')
  hm_parts <- append(hm_parts,list(baxis_grob))
  
  # Legends
  for (legend in legends) {
    hm_parts <- append(hm_parts,list(legend))
  }
  
  # RHS Dendro 
  if ( !is.null(row_dendro) ){
    hm_parts <- append.col.for.grob(extract.panel(row_dendro),hm_parts,pad_top,pad_bottom)
  } else {
    rowd.width=numeric(0)
  }
  
  lstrip_widths <- rep(lstrip.width/length(lstrip),length(lstrip))
  bstrip_heights <- rep(bstrip.height/length(bstrip),length(bstrip))
  
  panel_width <-  1 - laxis.width - sum(lstrip_widths) - sum(rowd.width)
  panel_height <- 1 - baxis.height - sum(bstrip_heights) - sum(cold.height)
  
  legend_heights <- rep(legend.height/length(legends),length(legends))
  
  cwidths = c(laxis.width,lstrip_widths,panel_width,rowd.width)
  cwidths = cwidths*(1/sum(cwidths))
  
  rheights = c(cold.height,panel_height,bstrip_heights,baxis.height,legend_heights)
  rheights = rheights*(1/sum(rheights))  
  
  hm <- gtable_matrix("heatmap",grobs=matrix(hm_parts,ncol=n_cols),
                      widths = unit(cwidths, "null"),
                      heights = unit(rheights, "null"))
  
  #     browser()
  
  hm <- gtable_add_padding(hm,unit(rep(1,len=4),"cm"))
  
  grid.newpage()    
  grid.draw(hm)
  return(hm)
}

extract.panel <- function(plot){
  gtable_filter(ggplotGrob(plot),'panel')  
}

extract.legend <- function(plot){
  gtable_filter(ggplotGrob(plot + theme(legend.position="bottom")),'guide-box')
}

# This is for centre row grobs that need top and bottom padding
# laxis, lstrip, row_dendro
#
append.col.for.grob <- function(grob,hm_list,pad_top,pad_bottom){
  for (i in seq(1,pad_top,len=pad_top)){
    hm_list <- append(hm_list,list(nullGrob()))
  }
  hm_list <- append(hm_list,list(grob))
  for (i in seq(1,pad_bottom,len=pad_bottom)) {
    hm_list <- append(hm_list,list(nullGrob()))
  }
  hm_list
}
