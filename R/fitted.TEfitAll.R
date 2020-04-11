

#' Get fitted values and summary statistics from a set of TEfit models
#'
#' @param TEs3s A set of models fit by TEfitAll()
#'
#' @export
#'
fitted.TEfitAll <- function(TEs3s){
  # loop through the fits, get the predVals, and calculate the mean/SE
  allPreds <- matrix(NA,length(TEs3s$allFitList),nrow(TEs3s$allFitList[[1]]$data)*10) ## make it plenty big for varieties of sizes
  maxTime <- 0
  for (curGroup in 1:length(TEs3s$allFitList)){
    curFit <- TEs3s$allFitList[[curGroup]]
    allPreds[curGroup,1:nrow(curFit$data)] <- curFit$model$fitVals
    if(maxTime<max(curFit$data[,2],na.rm=T)){maxTime <- max(curFit$data[,2],na.rm=T)}
  }
  allPreds <- allPreds[,apply(allPreds,2,function(x) !all(is.na(x)))]## trim the NAs
  meanPred <- apply(allPreds,2,mean,na.rm=T)
  ciPred   <- qnorm(.975)*(apply(allPreds,2,sd,na.rm=T)/sqrt(apply(allPreds,2,function(x) sum(!is.na(x)))))
  return(list(allPreds=allPreds,meanPred=meanPred,ciPred=ciPred,maxTime=maxTime))
}
