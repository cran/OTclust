#' Optimal Transport Alignment
#'
#' This function aligns an ensemble of partitions with a reference partition by optimal transport.
#' @param data -- a numeric matrix of horizontally stacked cluster labels. Each column contains cluster labels for all the data points according to one clustering result. The reference clustering result is put in the first column, and the first cluster must be labeled as 1.
#' @return a list of alignment result.
#' \item{distance}{Wasserstein distances between the reference partition and the others.}
#' \item{numcls}{the number of clusters for each partition.}
#' \item{statistics}{average tightness ratio, average coverage ratio, 1-average jaccard distance.}
#' \item{cap}{cluster alignment and points based (CAP) separability.}
#' \item{id}{switched labels.}
#' \item{cps}{covering point set.}
#' \item{match}{topological relationship statistics between the reference partition and the others.}
#' \item{Weight}{weight matrix.}
#' @examples
#' data(sim1)
#' # the number of clusters.
#' C = 4
#' # calculate baseline method for comparison.
#' kcl = kmeans(sim1$X,C)
#' # align clustering results for convenience of comparison.
#' compar = align(cbind(sim1$z,kcl$cluster))
#' @export

align <- function(data){
  if(!is.matrix(data)) stop('data must be a matrix\n')
  if(ncol(data)<2) stop('data must have at least 2 columns\n')
  if(min(data)<1) stop('the first cluster must be labeled as 1\n')
  nbs = ncol(data)
  res = ACPS(c(data)-1,nbs,1)
  cname = paste("C",1:res$numcls[1],sep="")
  rownames(res$statistics)=cname
  rownames(res$cap)=colnames(res$cap)=cname
  rownames(res$cps)=cname
  rownames(res$match)=cname
  colnames(res$weight)=cname
  weight=list()
  for(i in 1:(nbs-1)){
    weight[[i]] = res$weight[(sum(res$numcls[2:(i+1)])-res$numcls[i+1]+1):sum(res$numcls[2:(i+1)]),]
  }
  res$weight = weight
  return(res)
}
