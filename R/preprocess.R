#' Data preprocessing
#'
#' Preprocessing for dimension reduction based on variance, it will delete the variable whose variance is smaller than 0.5*mean variance of all variables.
#' @param data -- data that needs to be processed
#' @param l -- logical. If True, log-transformation will be carried out on the data.
#' @param pre -- logical. If True, pre-dimension reduction will be carried out based on the variance.
#' @return the processed data.
#' @examples
#' data(YAN)
#' preprocess(YAN,l=FALSE,pre=TRUE)
#' @export
preprocess <- function(data, l = TRUE, pre = TRUE){
  if(!is.matrix(data)) stop('data must be a matrix\n')
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
  data
}
