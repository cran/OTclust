##Calculate Gini index for each point (design for the apply fuction).
Gini <- function(x){
  sum(x*(1-x))
}

#' Point-wise Uncertainty Assessment
#'
#' Output both the numerical and graphical point-wise uncertainty assessment for each individual points. The return of clustCPS, visCPS or CPS can be directly used as the input.
#' @param result -- the return from function clustCPS, visCPS or CPS.
#' @param method -- method for calculating point-wise uncertainty. Using posterior probability matrix is \code{0} and using topological information between clusters is \code{1}. Default is using posterior probability matrix.
#' @return a list, in which P is the posterior probability matrix that each sample below to the reference clusters, point_stab is the point-wise stability for each sample and v is the visualization of the point-wise stability.
#' @examples
#' # CPS analysis on selection of visualization methods
#' data(vis_pollen)
#' k1=kmeans(vis_pollen$vis,max(vis_pollen$ref))$cluster
#' k2=kmeans(vis_pollen$vis,max(vis_pollen$ref))$cluster
#' k=cbind(as.matrix(k1,ncol=1),as.matrix(k2,ncol=1))
#' c=CPS(vis_pollen$ref, vis_pollen$vis, pert=k)
#' # Point-wise Uncertainty Assessment
#' pplot(c)
#' @export
pplot <- function(result, method=0){
  if(method==0){
    #### Using posterior probability matrix
    nbs=result$nEXP
    save=result$save
    n=length(save)/(nbs+1)
    K=result$numcls[-1]
    # normalize each row
    wt_row=result$weight
    wt_row=wt_row/ifelse(rowSums(wt_row)==0,1,rowSums(wt_row))
    P=matrix(0,n,result$numcls[1])
    for(i in 1:nbs){
      P_raw = matrix(0,n,K[i])
      for(j in 1:n){
        P_raw[j, save[j+i*n]+1] = 1
      }
      weight = wt_row[(sum(result$numcls[2:(i+1)])-result$numcls[i+1]+1):sum(result$numcls[2:(i+1)]),]
      P=P+P_raw%*%weight
    }
    P=P/nbs
    colnames(P)=seq(1,result$numcls[1],1)
    point_stab=1-apply(P,1,Gini)
  } else{
    #### Using topological information between clusters
    nbs=result$nEXP
    save=result$save
    n=length(save)/(nbs+1)
    K=result$numcls[-1]
    # normalize each row
    wt_row=(result$topo_result!=-1)+0
    wt_row=wt_row/ifelse(rowSums(wt_row)==0,1,rowSums(wt_row))
    wt_row[rowSums(wt_row)==0,]=rep(1/ncol(wt_row),ncol(wt_row))
    P=matrix(0,n,result$numcls[1])
    for(i in 1:nbs){
      P_raw = matrix(0,n,K[i])
      for(j in 1:n){
        P_raw[j, save[j+i*n]+1] = 1
      }
      weight = wt_row[(sum(result$numcls[2:(i+1)])-result$numcls[i+1]+1):sum(result$numcls[2:(i+1)]),]
      P=P+P_raw%*%weight
    }
    P=P/nbs
    colnames(P)=seq(1,result$numcls[1],1)
    point_stab=1-apply(P,1,Gini)
  }
  ## Visualize the point-wise stability in a plot
  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
  sc <- ggplot2::scale_colour_gradientn(colours = myPalette(1000))
  v=ggplot2::ggplot(mapping = ggplot2::aes(x = result$vis[,1], y = result$vis[,2], colour = point_stab)) +
    ggplot2::geom_point(size = 1, alpha=0.8) +
    sc +
    ggplot2::labs(x= "",y = "",colour = "Point-wise Stability") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())
  out=list(P=P,point_stab=point_stab,v=v)
}
