
# p <- ggheat(data[,sample_cols],cluster="",dist="") + geom_point(aes(y=iBAQ,x="Arms")) %>% 
#   add_genecontext() %>% 
#   add_samplecontext()

setClass("GGHeat",
         slots = list(data = "ANY",
                      main_plot = "ggORNULL"),
         prototype = list(data = NULL))

ggheat <- function(data,
                   distfun = dist,
                   hclustfun= hclust,
                   rlabels=rownames(data),
                   clabels=colnames(data),
                   cold=TRUE,rowd=TRUE,...){

  
  
  
  
  hmp <- ggplot(data) 
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

