#' Construct parameter terms for the double-rate change function
#'
#' NOTE: test more thoroughly.
#'
#' @param modList List of TEfit model details
#' @param pPrefix prefix for parameters (e.g., p or thresh)
#'
#' @export
#'

tef_getTermsExpoDouble <- function(modList,pPrefix){

  datTerms <- list()
  if(dim(modList$varIn)[2] >2){
    if(!exists(paste0(pPrefix,'Asym'),where=modList$covarTerms)){
      curFitAsym <- rep(T,length(modList$covars))}else{
        curFitAsym <- modList$covarTerms[[paste0(pPrefix,'Asym')]]
      }

    if(!exists(paste0(pPrefix,'Start'),where=modList$covarTerms)){
      curFitStart <- rep(T,length(modList$covars))}else{
        curFitStart <- modList$covarTerms[[paste0(pPrefix,'Start')]]
      }

    if(!exists(paste0(pPrefix,'RateA'),where=modList$covarTerms)){
      curFitRateA <- rep(T,length(modList$covars))}else{
        curFitRateA <- modList$covarTerms[[paste0(pPrefix,'RateA')]]
      }


    if(!exists(paste0(pPrefix,'RateB'),where=modList$covarTerms)){
      curFitRateB <- rep(F,length(modList$covars))}else{
        curFitRateB <- modList$covarTerms[[paste0(pPrefix,'RateB')]]
      }

    if(any(curFitAsym)){
      datTerms$pA <- paste0(paste0(pPrefix,'Asym_'),
                            c('0',
                              paste0(paste0(modList$covars[curFitAsym],'*'),
                                     modList$covars[curFitAsym])
                            ),collapse='+')}else{datTerms$pA <- paste0(pPrefix,'Asym')}

    if(any(curFitStart)){
      datTerms$pS <- paste0(paste0(pPrefix,'Start_'),
                            c('0',paste0(paste0(modList$covars[curFitStart],'*'),
                                         modList$covars[curFitStart])
                            ),collapse='+')}else{datTerms$pS <- paste0(pPrefix,'Start')}

    if(any(curFitRateA)){
      datTerms$pRa <- paste0(paste0(pPrefix,'RateA_'),
                            c('0',paste0(paste0(modList$covars[curFitRateA],'*'),
                                         modList$covars[curFitRateA])
                            ),collapse='+')}else{datTerms$pRa <- paste0(pPrefix,'RateA')}
    if(any(curFitRateB)){
      datTerms$pRb <- paste0(paste0(pPrefix,'RateB_'),
                            c('0',paste0(paste0(modList$covars[curFitRateB],'*'),
                                         modList$covars[curFitRateB])
                            ),collapse='+')}else{datTerms$pRb <- paste0(pPrefix,'RateB')}
  }else{
    datTerms$pA <- paste0(pPrefix,'Asym')
    datTerms$pS <- paste0(pPrefix,'Start')
    datTerms$pRa <- paste0(pPrefix,'RateA')
    datTerms$pRb <- paste0(pPrefix,'RateB')
  }

  if(pPrefix=='p'){ ## ## only if this is the parameter of interest
    for(curFix in modList$pFix){
      if(any(grep('Asym',names(modList$pFix)))){datTerms$pA <- modList$pFix[grep('Asym',names(modList$pFix))]}
      if(any(grep('Start',names(modList$pFix)))){datTerms$pS <- modList$pFix[grep('Start',names(modList$pFix))]}
      if(any(grep('RateA',names(modList$pFix)))){datTerms$pRa <- modList$pFix[grep('RateA',names(modList$pFix))]}
      if(any(grep('RateB',names(modList$pFix)))){datTerms$pRb <- modList$pFix[grep('RateB',names(modList$pFix))]}
    }
  }

  return(datTerms)

}
