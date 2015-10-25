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

#
# Slightly more complex example demonstrating how plots can be manipulated
# between calls to heatmap_components and show_ggheatmap
#

data(squid)
count_cols = c("Slime","VM","DM","Arms","Brain")

#
# stripcols argument appends extra columns into the tile dataframe
# these are available to add layers to tile prior to display
#
hmc <- heatmap_components(squid[,count_cols],stripdata = data.frame(iBAQ=squid[,1]))
hmc$tile + geom_point(aes(x="Slime",size=log(iBAQ)))
hm <- show_ggheatmap(hmc$tile,row_dendro = NULL,col_dendro = hmc$cold)


}
