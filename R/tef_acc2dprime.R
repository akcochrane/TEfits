#' Convert an ordered vector of accuracies to d-prime
#'
#' Given paired information regarding accuracy and stimulus presence,
#' run a Gaussian-weighted-mean smoother over the accuracy vector separately for
#' stimulus-present and stimulus-absent indices, then compute an index-wise d-prime.
#' Returning and entire-vector d-prime (i.e., stable across time) is also an option.
#'
#' @param accuracy     At each index, what was the accuracy: [bounded at 0 and 1]
#' @param stim_present At each index, was the stimulus present or absent: [binary; 0 and 1, or logical]
#' @param by_index     Should the d-prime be calculated for each index?
#' @param trial_hwhm   The Gaussian smoother has a half-width-half-max; values are given half weight at this index distance from the center index (as the smoother iterates through each index in turn as the center).
#' @param max_dprime   d-prime becomes infinite as accuracy approaches 0 or 1. This value limits the absolute value of d-prime.
#'
#' @export
#'
tef_acc2dprime <- function(accuracy,stim_present,by_index=T,trial_hwhm=3,max_dprime=5){
  stim_present <- as.numeric(stim_present)
  accuracy <- qnorm(-max_dprime/2)+accuracy*(1-2*(qnorm(-max_dprime/2)))

  stim_pres_acc <- stim_abs_acc <- rep(NA,length(accuracy))

  stim_pres_acc[stim_present==1] <- accuracy[stim_present==1]
  stim_abs_acc[stim_present==0] <- accuracy[stim_present==0]

  if(by_index==F){
    return(qnorm(mean(stim_pres_acc,na.rm=T))-qnorm(mean(1-stim_abs_acc,na.rm=T)))
  }

  pHit <- tef_runningMean(stim_pres_acc,k_hwhm = trial_hwhm)
  pFA  <- tef_runningMean(1-stim_abs_acc,k_hwhm = trial_hwhm)

  return(qnorm(pHit)-qnorm(pFA))

}
