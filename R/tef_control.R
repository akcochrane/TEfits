#' Get control parameters for a TEfit model
#'
#' \code{\link{TEfit}} internal
#'
#' @param quietErrs         logical. Should errors be printed to the Console?
#' @param suppressWarnings  logical. Should warnings be printed to the Console?
#' @param y_lim             Numeric vector of length 2. Lower and upper bounds of permitted predicted values.
#' @param rate_lim          Numeric vector of length 2. Lower and upper bounds of permitted rate values [log time constants].
#' @param shape_lim         Numeric vector of length 2. Lower and upper bounds of permitted shape parameter values (i.e., for Weibull).
#' @param expBase           For change functions with an exponential component, what should the base of the exponent be?
#' @param rateBase          What should the base of the rate exponent be?
#' @param pFix              Named numeric vector allowing specific parameters to be fixed to a constant (i.e., not estimated)
#' @param penalizeMean      Logical. Should the time-evolving model be penalized if the mean of the time-evolving predicted values diverges from the mean of the null [non-time-evolving] predicted values?
#' @param penalizeRate      Logical. Should the time-evolving model be penalized if the rate parameter is very near a boundary?
#' @param convergeTol       Convergence is extremely roughly defined in \code{TEfits} as the SD of the same estimated parameter on different runs with relatively low error. What should this SD be?
#' @param stepwise_asym     Logical. If a function will saturate by the end of the measurement time, this option allows the asymptote to be estimated from this time period (i.e., as stationary).
#' @param explicit          Character. Rather than using any of the pre-defined change or link functions, enter the specific function you want to test.
#' @param nTries            Numeric. What is the maximum number of optimization runs that should be attempted?
#'
#' @export
#'
tef_control <- function(quietErrs = F,
                        suppressWarnings = F,
                        nTries = 200,
                        y_lim = c(-1E7,1E7),
                        rate_lim = c(0,0),
                        shape_lim = c(0,0),
                        expBase = 2,
                        rateBase = 2,
                        pFix = c(),
                        penalizeMean = T,
                        penalizeRate = F,
                        convergeTol = 5E-2,
                        stepwise_asym = F,
                        explicit = ''
){

  # # ^ need a meanConstrain argument

  ## should calculate rate lims here, ideally

  return(list(
    quietErrs = quietErrs,
    suppressWarnings = suppressWarnings,
    nTries = nTries,
    y_lim = y_lim,
    rate_lim = rate_lim,
    shape_lim = shape_lim,
    expBase = expBase,
    rateBase = rateBase,
    pFix =  pFix,
    penalizeRate = penalizeRate,
    penalizeMean = penalizeMean,
    convergeTol = convergeTol,
    stepwise_asym = stepwise_asym,
    explicit = explicit
  ))
}
