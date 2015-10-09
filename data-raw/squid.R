library(ggplotheatmap)
library(devtools)
load("../../Projects/squidvenom/Squid_counts/countsViBAQ.RData")


count_cols = c("Slime","VM","DM","Arms","Brain")

# Remove bad rows
mc <- mc[which(!apply(mc[,count_cols],1,function(r) {sum(is.na(r))})>0),]


# Top100 Proteins
mcsorted <- mc[order(mc$iBAQ,decreasing=TRUE),]

top100 <- mcsorted[1:100,]

top100_counts <- scale(as.matrix(top100[,count_cols]))

hmc <- heatmap_components(as.data.frame(top100_counts),top100$comp_id)

iBAQ <- top100[hmc$data$row.ord,c("comp_id","iBAQ","prot_id","protein_group")]


top_labels <- top100[1:100,]$comp_id

dd <- hmc$data$tile
ddm <- merge(dd,iBAQ,by.x="rlabels",by.y="comp_id")

squid_c <- dcast(ddm,rlabels + iBAQ + prot_id + protein_group ~ clabels,value.var = "Intensity")

squid <- squid_c[,-1]
rownames(squid) <- squid_c[,1]

devtools::use_data(squid,overwrite = TRUE)


