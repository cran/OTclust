#' Visualize a partition on 2 dimensional space
#'
#' This function plots a partition on 2 dimensional reduced space.
#' @param data -- coordinates matrix of data.
#' @param labels -- cluster labels in a vector, the first cluster is labeled as 1.
#' @param convex.hull -- logical. If it is \code{True}, the plot draws convex hull for each cluster.
#' @param title -- title
#' @param xlab -- xlab
#' @param ylab -- ylab
#' @param legend.title -- legend title
#' @param legend.labels -- legend labels
#' @param add.text -- default True
#' @return none
#' @examples
#' data(sim1)
#' # the number of clusters.
#' C = 4
#' ens.data = ensemble(sim1$X[1:50,], nbs=50, clust_param=C, clustering="kmeans", perturb_method=1)
#'
#' # find mean partition and uncertainty statistics.
#' ota = otclust(ens.data)
#' # calculate baseline method for comparison.
#' kcl = kmeans(sim1$X[1:50],C)
#'
#' # align clustering results for convenience of comparison.
#' compar = align(cbind(sim1$z[1:50],kcl$cluster,ota$meanpart))
#' lab.match = lapply(compar$weight,function(x) apply(x,2,which.max))
#' kcl.algnd = match(kcl$cluster,lab.match[[1]])
#' ota.algnd = match(ota$meanpart,lab.match[[2]])
#' # plot the result on two dimensional space.
#' otplot(sim1$X[1:50,],ota.algnd,con=FALSE,title='Mean partition')   # mean partition by OTclust
#' @export

otplot <- function(data,labels,convex.hull=F,title="",xlab="",ylab="",legend.title="",legend.labels=NULL,add.text=T){
    if(!is.matrix(data)) stop('data must be a matrix\n')
    K = labels
    if(dim(data)[2]==2){
        Y = data.frame(PC1=data[,1],PC2=data[,2])
    } else if(dim(data)[2]>2){
        Y = as.data.frame(prcomp(data)$x[,1:2])
    }
    
    A = paste("C",as.character(K),sep="")
    B = as.numeric(factor(K))
    
    s = cbind(Y,A) %>% split(K)
    ch = s %>% lapply(function(el) chull(el$PC1, el$PC2))
    ch = do.call(rbind, lapply(names(ch), function(x) s[[x]][ch[[x]],]))
    
    mu_x = sapply(1:max(B),function(z){mean(data[which(B==z),1])})
    mu_y = sapply(1:max(B),function(z){mean(data[which(B==z),2])})
    
    if(is.null(legend.labels)){
        legend.labels = paste("C",sort(unique(K)),sep="")
    }
    if(add.text==T){
        text.labels = paste("C",sort(unique(K)),sep="")
    }else{
        text.labels = rep(NA,max(B))
    }
    
    if(convex.hull==F){
        ggplot2::ggplot(mapping=ggplot2::aes(x=Y[,1], y=Y[,2], color = A, shape=factor(K))) +
        ggplot2::geom_point(size=1, alpha = 0.5) +
        ggplot2::scale_color_manual(values=sort(unique(K))+1,
        labels=legend.labels) +
        ggplot2::theme_classic() +
        ggplot2::labs(x=xlab,y=ylab) +
        ggplot2::ggtitle(title) +
        ggplot2::theme(plot.title = ggplot2::element_text(face="bold", hjust = 0.5)) +
        ggplot2::guides(color=ggplot2::guide_legend(title=legend.title)) +
        ggplot2::annotate("text", x=mu_x, y=mu_y, label=text.labels, size=4, fontface = "bold")
        
    } else if(convex.hull==T){
        ggplot2::ggplot(mapping=ggplot2::aes(x=Y[,1], y=Y[,2], color = A)) +
        ggplot2::geom_point(shape=19, size=1, alpha=0.5) +
        ggplot2::geom_polygon(mapping=ggplot2::aes(fill = ch$A), alpha = 0.2) +
        ggplot2::theme_classic() +
        ggplot2::labs(x=xlab,y=ylab) +
        ggplot2::ggtitle(title) +
        ggplot2::theme(plot.title = ggplot2::element_text(face="bold", hjust = 0.5)) +
        ggplot2::guides(fill=ggplot2::guide_legend(title=legend.title), color=ggplot2::guide_legend(title=legend.title)) +
        ggplot2::annotate("text", x=mu_x, y=mu_y, label=text.labels, size=4, fontface = "bold")
    }
}
