#' Check for bound hitting and other undesireable outcomes within an optim() call
#'
#' TEfits internal.
#'
#' Sane boundaries for parameters are the only way that many nonlinear regression optimizations
#' can be identifiable. Fortunately, theory-driven constraints on parameter ranges provide useful
#' a priori restrictions on the possible ranges for parameters and model predictions.
#' This function checks the following:
#'  \itemize{
#' \item{\code{start and asymptote parameters} -- all models are parameterized in terms of
#' starting and ending values. This ensures that the starting and ending values comply with
#' the \emph{y_lim} boundaries; \emph{y_lim} may be user-defined, defined by another model feature (e.g.,
#' \emph{bernoulli} error function is limited to predicted values of 0 or 1; Weibull link thresholds must be
#' above 0).}
#' \item{\code{rate parameter} -- If not user-input, then defined by \code{TEfits::tef_getLinkedFun}.
#' Defaults, with exponential change, to a minimum that would provide 50% of model change in \code{sd(timeVar[1:10])}
#' amount of time, and to a maximum value that would provide 80% of model change at \code{max(timeVar)}. Other change
#' functions have limits that, with their respective parameterizations, are intended to imitate the limits of the
#' 3-parameter exponential (i.e., imitate the overall shape of the curve's extremes) \strong{These are
#' heuristics.} The default values are intended to be flexible while maintaining a sufficiently constrained curve such that
#' \emph{both} starting asymptote parameters are interpretable (i.e., if the rate parameter, which is a time constant,
#' were to be extremely small, the start parameter could become infinitely large or small). If a time-evolving process
#' occurs on a timescale that cannot be fit by the default boundaries, it is likely that the data is unsufficient to
#' characterize that process.}
#' \item{\code{pPrevTime parameter} -- 4-parameter Power change utilizes a so-called "previous learning time" parameter
#' that assumes that the learning function extents \emph{backward} through time. This parameter must be greater than
#' 0 and smaller than 1*10^5}
#' }
#'
#' Users are highly encouraged to use their own boundaries (e.g., \code{y_lim} & \code{rate_lim}), given knowledge of a specific dataset, using \code{\link{tef_control}}.
#'
#' @param err Error
#' @param guesses Parameter values
#' @param curDat Data being fit
#' @param pNames Parameter names
#' @param evalFun Function being fit
#' @param errFun Function to calculate error
#' @param respVar Name of the response variable
#' @param linkFunX If relevant, the "x" value for a link function (e.g., Weibull, logistic)
#' @param y_lim Limits to fit values
#' @param rate_lim Limits to rate parameter
#' @param shape_lim If using a Weibull change function, limits to Weibull shape parameter
#' @param penalizeRate Logical. Should error be penalized if rate is extremely close to the bounds?
#' @param paramTerms parameter-level regressions, to be evaluated for checking y_lim and rate_lim
#' @param guessGroups  deprecated
#'
#' @export
#'
tef_checkPars <- function(err,guesses,curDat,pNames,evalFun,errFun,respVar,linkFunX=NA,
                          y_lim,rate_lim,shape_lim,penalizeRate,paramTerms,
                          guessGroups=NULL){

  ########%#
  ### cycle through the parameter terms and check for the relevant limits for each
  #### (asymptote and start compared to y_lim, rate compared to rate_lim, and shape compared to shape_lim)
  if (exists('thresh_covars',paramTerms)){paramTerms <- paramTerms$thresh_covars}

  for (curCovar in names(paramTerms)){

    paramPred <- eval(formula(paste('~',paramTerms[curCovar]))[[2]],
                                envir=curDat)

    paramMinMax <- c(minVal = max(c(min(paramPred,na.rm=T), -1E10),na.rm = T),
                     maxVal = min(c(max(paramPred,na.rm=T), 1E10), na.rm=T)
                     ) # get minimum and maximum predicted values for the parameter

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
