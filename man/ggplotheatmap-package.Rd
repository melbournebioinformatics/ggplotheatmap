\name{ggplotheatmap-package}
\alias{ggplotheatmap-package}
\alias{ggplotheatmap}
\docType{package}
\title{
\packageTitle{ggplotheatmap}
}
\description{
\packageDescription{ggplotheatmap}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ggplotheatmap}
\packageIndices{ggplotheatmap}
This package provides two main functions, heatmap_components, and show_ggheatmap.
}
\author{
\packageAuthor{ggplotheatmap}

Maintainer: \packageMaintainer{ggplotheatmap}
}

\keyword{ package }
\examples{

# A very simple example
#
data(pms)

hmc <- heatmap_components(x = pms)

show_ggheatmap(hmc$tile,row_dendro = hmc$rowd,col_dendro = hmc$cold)

# Slightly more complex example demonstrating how plots can be manipulated
# between calls to heatmap_components and show_ggheatmap

td <- hmc$data$tile
ctd <- dcast(td,formula = rlabels ~ clabels,value.var='Intensity')
ctd[,-1] <- ctd[,-1] / ctd[, "X12.50"]


mtd <- melt(ctd,id.vars = "rlabels",value.name = "FoldChange",variable.name = "clabels")
hmc$tile <- hmc$tile + geom_point(data=mtd,aes(x=clabels,y=rlabels,size=FoldChange))

show_ggheatmap(hmc$tile,row_dendro = hmc$rowd,col_dendro = hmc$cold)

}
