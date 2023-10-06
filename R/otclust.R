#' Mean partition by optimal transport alignment.
#'
#' This function calculates the mean partition of an ensemble of partitions by optimal transport alignment and uncertainty/stability measures.
#' @param ensemble -- a matrix of ensemble partition. Use \code{ensemble()} to generate an ensemble of perturbed partitions.
#' @param idx -- an integer indicating the index of reference partition in \code{ensemble}. If not specified, median partition is used as the reference partition.
#' @return a list of alignment result.
#' \item{idx}{the index of reference partition.}
#' \item{avedist}{average distances between each partition and all ensemble partitions.}
#' \item{meanpart}{a list of mean partition.}
#' \item{distance}{Wasserstein distances between mean partition and the others.}
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
#' ens.data = ensemble(sim1$X[1:100,], nbs=10, clust_param=C, clustering="kmeans", perturb_method=1)
#' # find mean partition and uncertainty statistics.
#' ota = otclust(ens.data)
#' @export

otclust <- function(ensemble, idx=NULL){
  if(!is.matrix(ensemble)) stop('ensemble must be a matrix, which is the return of function ensemble\n')
  if((length(idx) != 0)&(length(idx) != 1)) stop('If provided, idx should be a positive integer\n')
  if(min(ensemble)<1) stop('the first cluster must be labeled as 1\n')
  n = nrow(ensemble)
  nbs = ncol(ensemble)
  
  avgdist = rep(NA,nbs)
  avgdist = ACPS(c(ensemble)-1,nbs,0)
  if(is.null(idx)){
    idx = which.min(avgdist$avedist)
  }
    
  ota.rp = align(cbind(ensemble[,idx],ensemble))

  K.rf = ota.rp$numcls[1]
  K.bs = ota.rp$numcls[-1]
  
  P.raw = list()
  for(i in 1:nbs){
    P.raw[[i]] = matrix(0,n,K.bs[i])
    for(j in 1:n){
      P.raw[[i]][j, ensemble[j,i]] = 1
    }
  }  
  
  wt = ota.rp$weight
  
  P.tild = matrix(NA,n,K.rf)
  P.tild.sum = matrix(0,n,K.rf)
  
  for(k in 1:nbs){
    wtsub = wt[[k]]
    P.tild = P.raw[[k]]%*%(wtsub/ifelse(rowSums(wtsub)==0,1,rowSums(wtsub)))
    P.tild.sum = P.tild.sum + P.tild
  }
  
  P.bar = P.tild.sum/nbs
  P.bar.hrd.asgn = apply(P.bar,1,which.max)
  K.mp = max(P.bar.hrd.asgn)
  
  ota.mp = c(idx=idx,avgdist,align(cbind(P.bar.hrd.asgn,ensemble)))
  
  ota.mp$meanpart = P.bar.hrd.asgn
  
  return(ota.mp)
}
