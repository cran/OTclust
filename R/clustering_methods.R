kclst <- function(X, newdata, clust_param){
  kcl = flexclust::kcca(X, k=clust_param[1], flexclust::kccaFamily("kmeans"))
  res = flexclust::predict(kcl, newdata=newdata)
  return(res)
}

kclst2 <- function(X, newdata, clust_param){
  kcl = kmeans(X, clust_param[1])
  res = as.numeric(class::knn(X,newdata,kcl$cluster))
  return(res)
}

mclst <- function(X, newdata, clust_param){
  mcl = mclust::Mclust(X, G=clust_param[1])
  res = predict(mcl, newdata=newdata)$classification
  return(res)
}

hclst <- function(X, newdata, clust_param){
  hcl = hclust(dist(X))
  res = as.numeric(class::knn(X,newdata,cutree(hcl,clust_param[1])))
  return(res)
}  

dclst <- function(X, newdata, clust_param){
  dcl = dbscan::dbscan(X, eps=clust_param[1])
  res = predict(dcl, newdata=newdata, data=X)
  return(res)
}


hmmvb <- function(X, newdat, clust_param){
  VbStructure <- clust_param[[1]]
  hmmTr <- HDclust::hmmvbTrain(X, VbStructure, trControl=HDclust::trainControl(diagCov=T))
  hmmCl <- HDclust::hmmvbClust(newdat, model=hmmvb, control = HDclust::clustControl(minSize=clust_param[[2]][[1]], modeTh = clust_param[[2]][[2]], useL1norm = T))
  res = hmmCl@clsid
  return(res)
}
