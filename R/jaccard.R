#' Jaccard similarity matrix.
#'
#' This function calculates Jaccard similarity matrix between two partitions.
#' @param x,y -- vectors of cluster labels
#' @return a matrix of Jaccard similarity between clusters in two partitions.
#' @examples
#' x=c(1,2,3)
#' y=c(3,2,1)
#' jaccard(x,y)
#' @export

jaccard <- function(x,y){
  mx = max(x)
  my = max(y)
  res = matrix(NA,mx,my)
  for(i in 1:mx){
    for(j in 1:my){
      res[i,j] = sum(table(c(which(x==i),which(y==j)))==2)/length(table(c(which(x==i),which(y==j))))
    }
  }
  return(res)
}
