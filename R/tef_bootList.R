#' Make a list of parameters for resampling a TEfit model
#'
#' \code{\link{TEfit}} internal.
#'
#' @param resamples   Number of resamples. More (e.g., >2000) is better, but requires more time and memory.
#' @param bootPercent Proportion of data to resample. Must be 0 < bootPercent <= 1. If 1, data is resampled repeatedly with replacement. If less than 1, that proportion of data is repeatedly randomly selected, the model is fit on the selected data, and fit indices (e.g., error) are calculated for the model predictions on the left-out data (i.e., cross-validation).
#' @param bootTries   Number of optimization runs for each resample
#'
#' @export
#'
tef_bootList <- function(resamples=0,bootPercent=1,bootTries=20){
  return(list(nBoots=resamples,
              bootPercent=bootPercent,
              bootTries=bootTries))
}
