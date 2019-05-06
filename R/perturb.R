#' Perturb data by adding noise or bootstrapping
#'
#' Perturb data by adding Gaussian noise or bootstrap resampling. Gaussian noise has mean 0 and variance 0.01*average variance of all variables.
#' @param data -- data that will be perturbed.
#' @param method -- adding noise is \code{0} and bootstrapping is \code{1}. Default is adding noise.
#' @return the perturbed data.
#' @examples
#' data(vis_pollen)
#' perturb(vis_pollen$vis,method=0)
#' @export
perturb <- function(data, method=0){
  if(method==1){
    ## Bootstrap resampling
    n = nrow(data)
    bet=sample(1:n,n,replace=T)
    pdata=data[bet,]
  } else {
    ## Adding noise
    pdata=apply(data,2,addnoise,nrow=nrow(data),sd=sqrt(0.1*mean(apply(data,2,var))))
  }
  
  return(pdata)
}
