

#' {{CREATE EXPLANATION}}
#'
#' Note that, despite the name, this is the getterms function for  *both* exponential and power [3-parameter] change functions
#'
#' @param modList {{CREATE EXPLANATION}}
#' @param pPrefix {{CREATE EXPLANATION}}
#'
#' @export
#'
#'

tef_getTermsExpo <- function(modList,pPrefix){

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

  }else{
    datTerms$pA <- paste0(pPrefix,'Asym')
    datTerms$pS <- paste0(pPrefix,'Start')
    datTerms$pR <- paste0(pPrefix,'Rate')
  }


  ## ##
  ## replace the parameter with a constant number, if input
  ## ##
  # if(pPrefix=='p'){ ## ## only if this is the parameter of interest # why??
  # for(curFix in modList$pFix){ #why?
    if(any(grep('Asym',names(modList$pFix)))){datTerms$pA <- modList$pFix[grep('Asym',names(modList$pFix))]}
    if(any(grep('Start',names(modList$pFix)))){datTerms$pS <- modList$pFix[grep('Start',names(modList$pFix))]}
    if(any(grep('Rate',names(modList$pFix)))){datTerms$pR <- modList$pFix[grep('Rate',names(modList$pFix))]}
  # }
    # }

  return(datTerms)

}
