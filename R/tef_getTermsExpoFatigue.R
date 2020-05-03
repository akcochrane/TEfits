

#' Define a time-dependent change function
#'
#' \code{\link{TEfit}} internal. Experimental. Exponential change followed by logistic decay.
#'
#' @param modList TEfit modList
#' @param pPrefix parameter prefix
#'
#' @export
#'
#'

tef_getTermsExpoFatigue <- function(modList,pPrefix){

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

    if(!exists(paste0(pPrefix,'FatigueAsym'),where=modList$covarTerms)){
      curFitFatigueAsym <- rep(T,length(modList$covars))}else{
        curFitFatigueAsym <- modList$covarTerms[[paste0(pPrefix,'FatigueAsym')]]
      }

    if(!exists(paste0(pPrefix,'FatigueHWHM'),where=modList$covarTerms)){
      curFitFatigueHWHM <- rep(T,length(modList$covars))}else{
        curFitFatigueHWHM <- modList$covarTerms[[paste0(pPrefix,'FatigueHWHM')]]
      }

    if(!exists(paste0(pPrefix,'FatigueTime'),where=modList$covarTerms)){
      curFitFatigueTime <- rep(T,length(modList$covars))}else{
        curFitFatigueTime <- modList$covarTerms[[paste0(pPrefix,'FatigueTime')]]
      }


    ##
    ## ##
    ##

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

    if(any(curFitFatigueAsym)){
      datTerms$pFatigueAsym <- paste0(paste0(pPrefix,'FatigueAsym_'),
                            c('0',paste0(paste0(modList$covars[curFitFatigueAsym],'*'),
                                         modList$covars[curFitFatiguAsym])
                            ),collapse='+')}else{datTerms$pFatigueAsym <- paste0(pPrefix,'FatigueAsym')}

    if(any(curFitFatigueTime)){
      datTerms$pFatigueTime <- paste0(paste0(pPrefix,'FatigueTime_'),
                                      c('0',paste0(paste0(modList$covars[curFitFatigueTime],'*'),
                                                   modList$covars[curFitFatigueTime])
                                      ),collapse='+')}else{datTerms$pFatigueTime <- paste0(pPrefix,'FatigueTime')}

    if(any(curFitFatigueHWHM)){
      datTerms$pFatigueHWHM <- paste0(paste0(pPrefix,'FatigueHWHM_'),
                                      c('0',paste0(paste0(modList$covars[curFitFatigueHWHM],'*'),
                                                   modList$covars[curFitFatigueHWHM])
                                      ),collapse='+')}else{datTerms$pFatigueHWHM <- paste0(pPrefix,'FatigueHWHM')}

    ##
    ## ##
    ##

  }else{
    datTerms$pA <- paste0(pPrefix,'Asym')
    datTerms$pS <- paste0(pPrefix,'Start')
    datTerms$pR <- paste0(pPrefix,'Rate')
    datTerms$pFatigueAsym <- paste0(pPrefix,'FatigueAsym')
    datTerms$pFatigueTime <- paste0(pPrefix,'FatigueTime')
    datTerms$pFatigueHWHM <- paste0(pPrefix,'FatigueHWHM')
  }


  ## ##
  ## replace the parameter with a constant number, if input
  ## ##
  # if(pPrefix=='p'){ ## ## only if this is the parameter of interest # why??
  for(curFix in modList$pFix){
    if(any(grep('Asym',names(modList$pFix)))){datTerms$pA <- modList$pFix[grep('Asym',names(modList$pFix))]}
    if(any(grep('Start',names(modList$pFix)))){datTerms$pS <- modList$pFix[grep('Start',names(modList$pFix))]}
    if(any(grep('Rate',names(modList$pFix)))){datTerms$pR <- modList$pFix[grep('Rate',names(modList$pFix))]}
    if(any(grep('FatigueAsym',names(modList$pFix)))){datTerms$pFatigueAsym <- modList$pFix[[grep('FatigueAsym',names(modList$pFix))]]}
    if(any(grep('FatigueTime',names(modList$pFix)))){datTerms$pFatigueTime <- modList$pFix[[grep('FatigueTime',names(modList$pFix))]]}
    if(any(grep('FatigueHWHM',names(modList$pFix)))){datTerms$pFatigueHWHM <- modList$pFix[[grep('FatigueHWHM',names(modList$pFix))]]}
  }
  # }

  return(datTerms)


  # dat <- data.frame(timeVar = 1:50, respVar = c(seq(.3,.9,length=25),seq(.9,.91,length=25))+rep(c(0,.01),25)-rep(c(0,0,0,0,.2),each=10),covar1=rep(c(1,2),25),covar2 = rep(c(-3,-1,0,2,5),10))

}
