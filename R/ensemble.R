#' Generate an ensemble of partitions.
#'
#' Generate multiple clustering results (that is, partitions) based on multiple versions of perturbed data using a specified baseline clustering method.
#' @param data -- data that will be perturbed.
#' @param nbs -- the number of clustering partitions to be generated.
#' @param clustering -- baseline clustering methods. User specified functions or example methods included in package ("kmeans", "Mclust", "hclust", "dbscan", "HMM-VB") can be used. Refer to the Detail.
#' @param clust_param -- parameters for pre-defined clustering methods. If clustering is "kmeans", "Mclust", "hclust", this is an integer indicating the number of clusters. For "dbscan", a numeric indicating epsilon. For "HMM-VB", a list of parameters.
#' @param perturb_method -- adding noise is \code{0} and bootstrap resampling is \code{1}. Default is bootstrap resampling.
#' # perturb_method=0 perturbed by adding Gaussian noise.
#' @return a matrix of cluster labels of the ensemble partitions. Each column is cluster labels of an individual clustering result.
#' @examples
#' data(sim1)
#' # the number of clusters.
#' C = 4
#' ens.data = ensemble(sim1$X[1:10,], nbs=10, clust_param=C, clustering="kmeans", perturb_method=1)
#' @export
#' @export
ensemble <- function(data, nbs, clust_param, clustering="kmeans", perturb_method=1){
  if(!is.matrix(data)) stop('data must be a matrix\n')
  if(is.function(clustering)){
    clustering = clustering
  } else if(is.character(clustering)){
    clustering = switch(clustering, 
                    "kmeans" = "kclst",
                    "Mclust" = "mclst",
                    "hclust" = "hclst",
                    "dbscan" = "dclst",
                    "HMM-VB" = "hmmvb2")
    if(is.null(clustering)){
      stop('Choose a clustering method from kmeans, Mclust, hclust, dbscan, HMM-VB, or specify a function.\n')
    } else {
      clustering = eval(parse(text=clustering))
    }
  } else {
    stop('Choose a clustering method from kmeans, Mclust, hclust, dbscan, HMM-VB, or specify a function.\n')
  }
  
  n = nrow(data)
  ens = matrix(NA,n,nbs)
  for (i in 1:nbs){
    pdat = perturb(data,perturb_method)
    ens[,i] = clustering(as.data.frame(pdat), newdata=as.data.frame(data), clust_param)
  }
  
  return(ens)
}
