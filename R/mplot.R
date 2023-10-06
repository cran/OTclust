#' Membership Heat Map
#'
#' Output the membership heat map of the required cluster. The return of clustCPS, visCPS or CPS can be directly used as the input.
#' @param result -- the return from function clustCPS, visCPS or CPS.
#' @param k -- which cluster that you want to see the membership heat map.
#' @return membership heat map of the required cluster.
#' @examples
#' # CPS analysis on selection of visualization methods
#' data(vis_pollen)
#' c=visCPS(vis_pollen$vis, vis_pollen$ref)
#' # visualization of the results
#' mplot(c,2)
#' cplot(c,2)
#' @export
mplot <- function(result, k){
  if((length(k) != 1) || k < 1) stop('k should be a positive integer >= 1!\n')
  col=matrix(0,ncol=1,nrow=nrow(result$vis))
  cc=function(x,a){
    sum(x==a)
  }
  sha=c(result$ref==k)
  sha[sha=="FALSE"]="OUT"
  sha[sha=="TRUE"]="IN"
  sha=factor(sha,levels = c("OUT","IN"))
  col=matrix(apply(result$member[,-1],1,cc,a=k),ncol=1)
  ggplot2::ggplot(mapping = ggplot2::aes(x = result$vis[,1], y = result$vis[,2], colour = col,shape=sha)) +
    ggplot2::geom_point(size = 2, alpha=0.8) +
    ggplot2::scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),limits=c(0,ncol(result$member)-1)) +
    ggplot2::labs(x= "",y = "",colour = "Membership",shape="Cluster Member") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())
}
