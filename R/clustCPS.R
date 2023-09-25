## Adding Gaussian noise col by col (design for the apply fuction).
addnoise <- function(x,nrow,sd){
  x+rnorm(nrow,0,sd)
}

#' CPS Analysis for cluster validation..
#'
#' Covering Point Set Analysis for validating clustering results. It conducts alignment among different results and then calculates the covering point set. The return contains several statistics which can be directly used as input for mplot or cplot. If you want to design your own workflow, you can use function CPS instead.
#' @param data -- data given in a matrix format, where rows are samples, and columns are variables.
#' @param k -- number of clusters.
#' @param l -- logical. If True, log-transformation will be carried out on the data.
#' @param pre -- logical. If True, pre-dimension reduction will be carried out based on the variance.
#' @param noi -- adding noise before or after the dimension reduction, choosing between "before" and "after", default "after".
#' @param cmethod -- clustering method, choosing from "kmeans" and "mclust", default "kmeans".
#' @param dimr -- dimension reduction technique, choose from "none" and "PCA", default "PCA".
#' @param vis -- the visualization method to be used, such as "tsne" and "umap", default "tsne". Also, you can provide your own visualization coordinates in a numeric matrix of two columns.
#' @param ref -- optional, clustering result in a vector format and the first cluster is labeled as 1. If provided it will be used as the reference, if not we will generate one.
#' @param nPCA -- number of principal components to use, default 50.
#' @param nEXP -- number of perturbed clustering results for CPS Analysis, default 100.
#' @return a list used for mplot or cplot, in which tight_all is the overall tightness, member is the matrix used for the membership plot, set is the matrix for the covering point set plot, tight is the vector of cluster-wise tightness, vis is the visualization coordinates, ref is the reference labels and topo is the topological relationship between clusters for point-wise uncertainty assessment.
#' @examples
#' # CPS Analysis on validation of clustering result
#' data(YAN)
#' # Suppose you generate the visualization coordinates on your own
#' x1=matrix(seq(1,nrow(YAN),1),ncol=1)
#' x2=matrix(seq(1,nrow(YAN),1),ncol=1)
#' # Using nEXP=50 for illustration, usually use nEXP greater 100
#' y=clustCPS(YAN[,1:100], k=7, l=FALSE, pre=FALSE, noi="after",vis=cbind(x1,x2), nEXP = 50)
#' # visualization of the results
#' mplot(y,4)
#' @export
clustCPS <- function(data, k, l = TRUE, pre = TRUE, noi="after", cmethod="kmeans", dimr="PCA", vis="tsne", ref = NULL, nPCA = 50, nEXP = 100){
  if(!is.matrix(data)) stop('data must be a matrix\n')
  if((length(k) != 1) || k < 2) stop('k should be a positive integer >= 2!\n')
  if(!is.null(ref) & (min(ref)!=1)) stop('If provide, ref should be a numeric vector and the first cluster is labeled as 1!\n')
  ## log-transfromation
  if(l) {data=log2(as.matrix(data) + 1)}
  ## pre-dimention reduction
  if(pre) {
    screen=matrix(0,nrow=1,ncol=ncol(data))
    for(i in 1:ncol(data)){
      screen[1,i]=var(data[,i])
    }
    data=data[,screen > 0.5*mean(apply(data,2,var))]
  }
  ## generate the visualizatoin coordinates
  if(vis[1]=="tsne"){
    v=tsne::tsne(data)
  } else if(vis[1]=="umap"){
      v=umap::umap(data)$layout
  } else if(is.numeric(vis)&(ncol(vis)==2)){
      v=vis
  } else {stop('Please provide valid visulization method or coordinates!\n')}
  ## PCA
  if(dimr=="PCA"){
    pca=prcomp(data)$x[,1:nPCA]
  }else if(dimr=="none"){pca=data}
  ## generate a reference clustering result if needed
  if(length(ref)==0){
    if(cmethod=="kmeans"){
      ref=kmeans(pca,k,iter.max=150,algorithm="MacQueen")
      ref=matrix(ref$cluster,ncol=1)
    }else if(cmethod=="mclust"){
      ref=mclust::Mclust(pca,k)
      ref=matrix(ref$classification,ncol=1)
    }
  }
  if(min(ref)<1) stop('the first cluster must be labeled as 1\n')
  ## generate perturbed data then cluster
  # calculate the average within-cluster variance of the clustering result
  re=matrix(0,ncol=nEXP,nrow=nrow(pca))
  nsd_before=0
  nsd_after=0
  for(i in 1:k){
    nsd_before=nsd_before+var(as.vector(data[ref==i,]))
    nsd_after=nsd_after+var(as.vector(pca[ref==i,]))
  }
  nsd_after=nsd_after/k
  nsd_before=nsd_before/k
  for(i in 1:nEXP){
    ## Adding noise
    if(noi=="before"){
      if(dimr=="none"){
        print("Can not do it without dimension reduction method PCA")
        break
      }
      inp=data
      toy=apply(inp,2,addnoise,nrow=nrow(inp),sd=sqrt(0.1*nsd_before))
      toy=prcomp(toy)$x[,1:nPCA]
    }else if(noi=="after"){
      inp=pca
      toy=apply(inp,2,addnoise,nrow=nrow(inp),sd=sqrt(0.1*nsd_after))
    }
    ## Kmeans or Mclust on perturbed data
    if(cmethod=="kmeans"){
      kk=kmeans(toy,k,iter.max=150,algorithm="MacQueen")
      re[,i]=matrix(kk$cluster,ncol=1)
    }else if(cmethod=="mclust"){
      kk=mclust::Mclust(toy,k)
      re[,i]=matrix(kk$classification,ncol=1)
    }
  }
  ## CPS Analysis
  save=rbind(matrix(as.integer(ref)-1,ncol=1),matrix(as.integer(re)-1,ncol=1))
  cps=ACPS(save,nEXP+1,1)
  pen=cps$match[,1]/apply(cps$match,1,sum)
  tit=cps$statistics[,4]*pen
  tit=matrix(tit,nrow=1)
  rownames(tit)=c("Tightness of each cluster")
  colnames(tit)=seq(1,k,1)
  ## output
  tight_all=mean(tit)
  member=cps$id
  set=t(cps$cps)
  tight=tit
  topo=cps$topo_result
  out=list(tight_all=tight_all, member=member, set=set, tight=tight, vis=v, ref=ref, topo=topo, numcls=cps$numcls, nEXP=nEXP, save=save, weight=cps$weight)
}
