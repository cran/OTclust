#' Covering Point Set Plot
#'
#' Output the Covering Point Set plot of the required cluster. The return of clustCPS, visCPS or CPS can be directly used as the input.
#' @param result -- the return from function clustCPS, visCPS or CPS.
#' @param k -- which cluster that you want to see the covering point set plot.
#' @return covering point set plot of the required cluster.
#' @examples
#' # CPS analysis on selection of visualization methods
#' data(vis_pollen)
#' c=visCPS(vis_pollen$vis, vis_pollen$ref)
#' # visualization of the results
#' mplot(c,2)
#' cplot(c,2)
#' @export
cplot <- function(result, k){
  if((length(k) != 1) || k < 1) stop('k should be a positive integer >= 1!\n')
  colo=matrix("OUT",ncol=1,nrow=nrow(result$vis))
  cpsk=which(result$set[,k]!=0)
  colo[cpsk]="IN"
  sha=c(result$ref==k)
  sha[sha=="FALSE"]="OUT"
  sha[sha=="TRUE"]="IN"
  sha=factor(sha,levels = c("OUT","IN"))
  ggplot2::ggplot(mapping = ggplot2::aes(x = result$vis[,1], y = result$vis[,2],colour = as.factor(colo),shape=sha)) +
    ggplot2::scale_color_manual(values = c("#9E0142","#5E4FA2")) +
    ggplot2::geom_point(size = 2,alpha=0.8) +
    ggplot2::labs(x= "",y = "",shape="Cluster Member",colour= "CPS")+ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                    axis.text.x=ggplot2::element_blank(),
                    axis.ticks.x=ggplot2::element_blank(),
                    axis.title.y=ggplot2::element_blank(),
                    axis.text.y=ggplot2::element_blank(),
                    axis.ticks.y=ggplot2::element_blank())
}
