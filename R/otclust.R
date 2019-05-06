#' Mean partition by optimal transport anlignment.
#'
#' This function calculates the mean partition of an ensemble of partitions by optimal transport alignment and uncertainty/stability measures.
#' @param ensemble -- a matrix of ensemble partition. Use \code{ensemble()} to generate an ensemble of perturbed partitions.
#' @param idx -- an integer indicating the index of reference partition in \code{ensemble}. If not specified, median partition is used as the reference partition.
#' @return a list of mean partition(meanpart), distance of mean partition to other partitions in ensemble(distance), weight matrix(weight), topological stability statistics(match), covering point set(cps), cluster alignment and point-based separability(cap).
#' @examples
#' data(sim1)
#' # the number of clusters.
#' C = 4
#' ens.data = ensemble(sim1$X, nbs=8, clust_param=C, clustering="kmeans", perturb_method=1)
#' # find mean partition and uncertainty statistics.
#' ota = otclust(ens.data)
#' @export

otclust <- function(ensemble, idx=NULL){
  n = nrow(ensemble)
  nbs = ncol(ensemble)
  
  if(is.null(idx)){
    avgdist = rep(NA,nbs)
    for(i in 1:nbs){
      repre = ensemble[,i]
      avgdist[i] = mean(ACPS(c(repre,ensemble)-1,nbs+1)$distance)
      print(i)
    }
    idx = which.min(avgdist)
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
  
  ota.mp = align(cbind(P.bar.hrd.asgn,ensemble))
  
  ota.mp$meanpart = P.bar.hrd.asgn
  
  return(ota.mp)
}
