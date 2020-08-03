#' Extract coefficients from a TEfit model
#'
#' @param TEs3 A TEfit model, possibly with resampled parameters
#'
#' @method coef TEfit
#' @export
#'
#' @examples
#' \dontrun{
#' m <- TEfit(anstrain_s1[,c('acc','trialNum')], bootPars = tef_bootList(resamples = 50))
#' coef(m)
#' }
#'
coef.TEfit <- function(TEs3){
  parVals <- data.frame(Estimate=round(TEs3$model$par,3))

  if(exists('bootList',TEs3)){
    nPars <- ncol(TEs3$bootList$boots)-4

    bootCoef <- data.frame(Q025 = round(TEs3$bootList$boots['2.5%',1:nPars],3),
    Q975 =round(TEs3$bootList$boots['97.5%',1:nPars],3))

    parVals <- merge(parVals,bootCoef,all=T,by=0)

    parVals$pseudoSE = round((
      (parVals$Q975-parVals$Estimate)/qnorm(.975)+
      (parVals$Q025-parVals$Estimate)/qnorm(.025)
      )/2,3)
     rownames(parVals) <- parVals$Row.names
  parVals$Row.names <- NULL
  }

  return(parVals)
}
