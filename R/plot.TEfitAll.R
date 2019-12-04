
#' Plot a collection of models fit by TEfitAll()
#'
#' @param TEs3s A TEfitAll model object (collection of fit models)
#' @param ymin minimum y value
#' @param ymax maximum y value
#' @param returnSummary Return the predicted values?
#'
#' @export
#'
plot.TEfitAll <- function(TEs3s,ymin=NA,ymax=NA){

  fits <- fitted(TEs3s)
  curFit <- TEs3s$allFitList[[1]]
  globalTime <- seq(min(curFit$data[,2],na.rm=T),
                    fits$maxTime,
                    length=length(fits$meanPred))

  # create the empty plot
  if(is.na(ymin)){ymin=min(fits$meanPred-fits$ciPred)}
  if(is.na(ymax)){ymax=max(fits$meanPred+fits$ciPred)}
  par(family='serif')
  plot(c(min(c(0,min(curFit$data[,2]))),max(curFit$data[,2]))
    ,round(c(min(fits$meanPred-fits$ciPred),max(fits$meanPred+fits$ciPred)),1)
    ,ylim=c(ymin,ymax)
    ,col='white'
    ,xlab=curFit$modList$timeVar
    ,ylab=curFit$modList$respVar
    ,main=paste('Time-related fits\nby',attr(TEs3s$fitSummary,'grouping_var'))
    ,sub = 'Dashed lines indicate 95% CI'
  )

  # loop through the fits and plot light lines
  for(curGroup in 1:nrow(fits$allPreds)){
    lines(TEs3s$allFitList[[curGroup]]$data[,2],TEs3s$allFitList[[curGroup]]$data$fitVals,
          col='lightgrey')}


  # plot mean/SE
  lines(globalTime,fits$meanPred,col='darkred',lwd=3)
  lines(globalTime,fits$meanPred-fits$ciPred,col='darkred',lwd=2,lty=3)
  lines(globalTime,fits$meanPred+fits$ciPred,col='darkred',lwd=2,lty=3)

  # if(returnSummary){ ## deprecated
  #   return(list(meanFit=fits$meanPred,ciFit=fits$ciPred,allFits=fits$allPreds))
  # }

}
