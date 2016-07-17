

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


add_clusterby_aes <- function(mapping){
  if (!is.null(mapping)){
    mapping <- aes.update(mapping,cluster_by=value)
  } else {
    mapping <-  aes(cluster_by=value)
  }
  mapping
}

aes.update <- function (orig, ...) 
{
  aes_new <- structure(as.list(match.call()[-c(1,2)]), class="uneval")
  aes_new <- ggplot2:::rename_aes(aes_new)
  orig[names(aes_new)] <- aes_new
  orig
}
