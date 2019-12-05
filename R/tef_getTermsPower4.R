

#' Define a time-dependent change function
#'
#' Internal to TEfits. 4-parameter power function.
#'
#' @param modList TEfit modList
#' @param pPrefix parameter prefix
#'
#' @export
#'
#'

tef_getTermsPower4 <- function(modList,pPrefix){

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

    if(!exists(paste0(pPrefix,'Rate'),where=modList$covarTerms)){
      curFitRate <- rep(T,length(modList$covars))}else{
        curFitRate <- modList$covarTerms[[paste0(pPrefix,'Rate')]]
      }


    if(!exists(paste0(pPrefix,'PrevTime'),where=modList$covarTerms)){
      curFitPrevTime <- rep(T,length(modList$covars))}else{
        curFitPrevTime <- modList$covarTerms[[paste0(pPrefix,'PrevTime')]]
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

    if(any(curFitRate)){
      datTerms$pR <- paste0(paste0(pPrefix,'Rate_'),
                            c('0',paste0(paste0(modList$covars[curFitRate],'*'),
                                         modList$covars[curFitRate])
                            ),collapse='+')}else{datTerms$pR <- paste0(pPrefix,'Rate')}

    if(any(curFitPrevTime)){
      datTerms$pPrevTime <- paste0(paste0(pPrefix,'PrevTime_'),
                            c('0',paste0(paste0(modList$covars[curFitPrevTime],'*'),
                                         modList$covars[curFitPrevTime])
                            ),collapse='+')}else{datTerms$pPrevTime <- paste0(pPrefix,'PrevTime')}

  }else{
    datTerms$pA <- paste0(pPrefix,'Asym')
    datTerms$pS <- paste0(pPrefix,'Start')
    datTerms$pR <- paste0(pPrefix,'Rate')
    datTerms$pPrevTime <- paste0(pPrefix,'PrevTime')
  }

  # print(datTerms)

  ## ##
  ## replace the parameter with a constant number, if input
  ## ##
  # if(pPrefix=='p'){ ## ## only if this is the parameter of interest #why?
  # for(curFix in modList$pFix){ # why?
    if(any(grep('Asym',names(modList$pFix)))){datTerms$pA <- modList$pFix[grep('Asym',names(modList$pFix))]}
    if(any(grep('Start',names(modList$pFix)))){datTerms$pS <- modList$pFix[grep('Start',names(modList$pFix))]}
    if(any(grep('Rate',names(modList$pFix)))){datTerms$pR <- modList$pFix[grep('Rate',names(modList$pFix))]}
    if(any(grep('PrevTime',names(modList$pFix)))){datTerms$pPrevTime <- modList$pFix[grep('PrevTime',names(modList$pFix))]}
  # }
  # }



  return(datTerms)

}
