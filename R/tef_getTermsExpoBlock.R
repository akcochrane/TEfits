
#' {{CREATE EXPLANATION}}
#'
#' pBlScale indicates the warping of distance-to-asymptote based on block trial number.
#'
#' @param modList List of TEfit model details
#' @param pPrefix {{CREATE EXPLANATION}}
#'
#' @export
#'

tef_getTermsExpoBlock <- function(modList,pPrefix){

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


    if(!exists(paste0(pPrefix,'BlScale'),where=modList$covarTerms)){
      curFitBlScale <- rep(T,length(modList$covars))}else{
        curFitBlScale <- modList$covarTerms[[paste0(pPrefix,'BlScale')]]
      }

    if(!exists(paste0(pPrefix,'BlRate'),where=modList$covarTerms)){
      curFitBlRate <- rep(T,length(modList$covars))}else{
        curFitBlRate <- modList$covarTerms[[paste0(pPrefix,'BlRate')]]
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

    if(any(curFitBlScale)){
      datTerms$pBS <- paste0(paste0(pPrefix,'BlScale_'),
                            c('0',paste0(paste0(modList$covars[curFitBlScale],'*'),
                                         modList$covars[curFitBlScale])
                            ),collapse='+')}else{datTerms$pBS <- paste0(pPrefix,'BlScale')}

    if(any(curFitBlRate)){
      datTerms$pBR <- paste0(paste0(pPrefix,'BlRate_'),
                            c('0',paste0(paste0(modList$covars[curFitBlRate],'*'),
                                         modList$covars[curFitBlRate])
                            ),collapse='+')}else{datTerms$pBR <- paste0(pPrefix,'BlRate')}

  }else{
    datTerms$pA <- paste0(pPrefix,'Asym')
    datTerms$pS <- paste0(pPrefix,'Start')
    datTerms$pR <- paste0(pPrefix,'Rate')
    datTerms$pBS <- paste0(pPrefix,'BlScale')
    datTerms$pBR <- paste0(pPrefix,'BlRate')
  }

  if(pPrefix=='p'){ ## ## only if this is the parameter of interest
    for(curFix in modList$pFix){
      if(any(grep('Asym',names(modList$pFix)))){datTerms$pA <- modList$pFix[grep('Asym',names(modList$pFix))]}
      if(any(grep('Start',names(modList$pFix)))){datTerms$pS <- modList$pFix[grep('Start',names(modList$pFix))]}
      if(any(grep('Rate',names(modList$pFix)))){datTerms$pR <- modList$pFix[grep('Rate',names(modList$pFix))]}
      if(any(grep('BlScale',names(modList$pFix)))){datTerms$pBS <- modList$pFix[grep('BlScale',names(modList$pFix))]}
      if(any(grep('BlRate',names(modList$pFix)))){datTerms$pBR <- modList$pFix[grep('BlRate',names(modList$pFix))]}
    }
  }

  return(datTerms)

}
