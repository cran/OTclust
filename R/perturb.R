#' Perturb data by adding noise, bootstrapping or mix-up
#'
#' Perturb data by adding Gaussian noise, bootstrap resampling or mix-up. Gaussian noise has mean 0 and variance 0.01*average variance of all variables. The mix-up lambda is 0.9.
#' @param data -- data that will be perturbed.
#' @param method -- adding noise is \code{0}, bootstrapping is \code{1} and mix-up is \code{2}. Default is adding noise.
#' @return the perturbed data.
#' @examples
#' data(vis_pollen)
#' perturb(as.matrix(vis_pollen$vis),method=0)
#' @export
perturb <- function(data, method=0){
  if(!is.matrix(data)) stop('data must be a matrix\n')
  if(method==1){
    ## Bootstrap resampling
    n = nrow(data)
    bet=sample(1:n,n,replace=T)
    pdata=data[bet,]
  } else if(method==2) {
    ## Mixing-up
    lambda=0.9
    mix_id=sample(seq(1,nrow(data)),size=nrow(data),replace = TRUE)
    mdata=data[mix_id,]
    pdata=lambda*data+(1-lambda)*mdata
  } else {
    ## Adding noise
    pdata=apply(data,2,addnoise,nrow=nrow(data),sd=sqrt(0.1*mean(apply(data,2,var))))
  }
  return(pdata)
}
