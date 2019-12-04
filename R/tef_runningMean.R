#' 1-dimensional Gaussian smoothing
#'
#' Runs a Gaussian or Cauchy density estimate centered over each
#' element [numeric, logical, or NA]
#' of the vector, and calculates
#' a density-weighted average for that element's index.
#'
#' @param x vector to be smoothed
#' @param k_hwhm The half-width half-max of the kernal [when Gaussian = sd*1.17741]. This is the index distance at which an element receives half the weight of the element at the center of the smooothing.
#' @param distr The distribition's density function to be used. 'gaussian' or 'cauchy'
#'
#' @export
#'
tef_runningMean <- function(x,k_hwhm=2,distr='gaussian'){
  k_sd <- k_hwhm*.8493218
  mW <- rep(NA,length(x))
  for (curX in 1:length(x)){
    if(distr=='cauchy'){
      curD <- dcauchy(1:length(x),curX,k_hwhm)}else{
        curD <- dnorm(1:length(x),curX,k_sd)}
    mW[curX] <- weighted.mean(x,curD,na.rm = T)
  }
  return(mW)
}
