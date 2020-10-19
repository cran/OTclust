#' Wasserstein distance between two partitions.
#'
#' This function calculates Wasserstein distance between two partitions.
#' @param x,y -- vectors of cluster labels
#' @return a distance between 0 and 1.
#' @examples
#' x=c(1,2,3)
#' y=c(3,2,1)
#' wassDist(x,y)
#' @export

wassDist <- function(x,y){
  if(min(c(x,y))<1) stop('the first cluster must be labeled as 1\n')
  ACPS(c(x,y),2,1)$distance[2]
}
