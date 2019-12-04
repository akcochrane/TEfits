#' {{CREATE EXPLANATION}}
#'
#' @export
#'
tef_makeErrTable <- function(){
  errTable <- data.frame(
    smallVariance = 'The variation in the response variable is very small. Reconsider the appropriateness of predicting it. '
    ,nonBinary = 'The response variable is not bounded by [or exactly] 0 or 1. Results may be unstable. '
    ,smallTime = 'The number of time points is small. Results may be unstable. Consider restricting the rate parameter. '
  )
  return(errTable)
}
