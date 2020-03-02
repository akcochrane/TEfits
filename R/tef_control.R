#' Get control parameters for a TEfit model
#'
#' @param quietErrs ..
#' @param suppressWarnings ..
#' @param modList ..
#' @param y_lim ..
#' @param rate_lim ..
#' @param expBase ..
#' @param rateBase ..
#' @param pFix ..
#' @param penalizeMean ..
#' @param penalizeRate ..
#' @param convergeTol ..
#' @param stepwise_asym ..
#' @param explicit ..
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
