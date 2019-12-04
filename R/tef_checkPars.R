#' Check for bound hitting and other undesireable outcomes within an optim() call
#'
#' @param err ?
#' @param guesses ?
#' @param curDat ?
#' @param pNames ?
#' @param evalFun ?
#' @param errFun ?
#' @param respVar ?
#' @param linkFunX?
#' @param y_lim ?
#' @param rate_lim ?
#' @param shape_lim ?
#' @param penalizeRate ?
#' @param paramTerms ?
#' @param guessGroups  ?
#'
#' @export
#'
tef_checkPars <- function(err,guesses,curDat,pNames,evalFun,errFun,respVar,linkFunX=NA,
                          y_lim,rate_lim,shape_lim,penalizeRate,paramTerms,
                          guessGroups=guessGroups){

  ########%#
  ### cycle through the parameter terms and check for the relevant limits for each
  #### (asymptote and start compared to y_lim, rate compared to rate_lim, and shape compared to shape_lim)
  if (exists('thresh_covars',paramTerms)){paramTerms <- paramTerms$thresh_covars}
  for (curCovar in names(paramTerms)){
    paramMinMax <- fivenum(eval(formula(paste('~',paramTerms[curCovar]))[[2]],
                                envir=curDat))[c(1,5)] # get minimum and maximum predicted values for the parameter
    ########%#
    # as yet unused:'pBS','pFatigueTime','pFatigueHWHM'
    ########%#
    if(is.na(linkFunX)){# this still has the age-old problem: ylim is only ok for non-linked functions
      if(curCovar=='pA' || curCovar=='pS' || curCovar== 'pFatigueAsym'){
        if(paramMinMax[1]<y_lim[1] || paramMinMax[2]>y_lim[2]){err <- 1E15}
      }}
    if(curCovar=='pR' || curCovar=='pRa' || curCovar== 'pRb' || curCovar== 'pBR'){
      if(paramMinMax[1]<rate_lim[1] || paramMinMax[2]>rate_lim[2]){err <- 1E15}

      if(penalizeRate){
        err <-  max(c(
          tef_penalizedErr(boundedPar=paramMinMax[1],errPar=err,loBound=rate_lim[1],upBound=rate_lim[2]),
          tef_penalizedErr(boundedPar=paramMinMax[2],errPar=err,loBound=rate_lim[1],upBound=rate_lim[2])
        ))
        }
    }
    if(curCovar== 'pShape'){
      if(paramMinMax[1]<shape_lim[1] || paramMinMax[2]>shape_lim[2]){err <- 1E15}
    }
    if(curCovar== 'pPrevTime'){
      if(paramMinMax[1]<0 || paramMinMax[2]>1E5){err <- 1E15}
    }
  }
  ########%#

  ########%#
  ####  in the future, should be parameterizable.. also include other links, etc.
  ########%#
  if(any(names(guesses)=='weibull_shape')){if(guesses['weibull_shape'] < 0 || guesses['weibull_shape'] > 10){err <- 1E15}}

  return(err)
}
