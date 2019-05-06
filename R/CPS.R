#' @import stats

#' @import magrittr

#' @importFrom grDevices chull

#' @useDynLib OTclust
#' @importFrom Rcpp sourceCpp
NULL

#' CPS Analysis on a collection of clustering results
#'
#' Covering Point Set Analysis of given clustering results. It conducts alignment among different results and then calculates the covering point set. The return contains several statistics which can be directly used as input for mplot or cplot. By using this function you can design your own workflow instead od using clustCPS, see vignette for more destails.
#' @param ref -- the reference clustering result, the first cluster is labeled as 1.
#' @param vis -- the visualization coordinates.
#' @param pert -- a collection of clustering results in a matrix format, each column represents one clustering result.
#' @return a list used for mplot or cplot, in which tight_all is the overall tightness, member is the matrix used for the membership heat map, set is the matrix for the covering point set plot, tight is the vector of cluster-wise tightness, vis is the visualization coordinates, and ref is the reference labels.
#' @examples
#' # CPS analysis on selection of visualization methods
#' data(vis_pollen)
#' k1=kmeans(vis_pollen$vis,max(vis_pollen$ref))$cluster
#' k2=kmeans(vis_pollen$vis,max(vis_pollen$ref))$cluster
#' k=cbind(as.matrix(k1,ncol=1),as.matrix(k2,ncol=1))
#' c=CPS(vis_pollen$ref, vis_pollen$vis, pert=k)
#' # visualization of the results
#' mplot(c,2)
#' cplot(c,2)
#' @export
CPS <- function(ref, vis, pert){
  ## CPS Analysis
  k=max(ref)
  nEXP=ncol(pert)
  save=rbind(matrix(as.integer(ref)-1,ncol=1),matrix(as.integer(pert)-1,ncol=1))
  cps=ACPS(save,nEXP+1)
  pen=1-cps$match[,4]/apply(cps$match,1,sum)
  tit=cps$statistics[,4]*pen
  tit=matrix(tit,nrow=1)
  rownames(tit)=c("Tightness of each cluster")
  colnames(tit)=seq(1,k,1)
  ## output
  tight_all=mean(tit)
  member=cps$id
  set=t(cps$cps)
  tight=tit
  v=vis
  out=list(tight_all=tight_all, member=member, set=set, tight=tight, vis=v, ref=ref)
}


