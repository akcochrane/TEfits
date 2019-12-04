#' Extract residuals from a TEfit model
#'
#' Plotting of the residuals is also an option.
#'
#' @param modelIn A TEfit model
#'
#' @export
#'
residuals.TEfit <- function(modelIn,toPlot=F){

  modelResids <- modelIn$modell$model_residuals
  nullResids <- modelIn$modList$varIn[,modelIn$modList$respVar]-mean(modelIn$modList$varIn[,modelIn$modList$respVar],na.rm=T)

  if(toPlot){
    par(mfrow=c(3,1))
    plot(modelIn$modList$varIn[,modelIn$modList$timeVar],modelIn$modList$varIn[,modelIn$modList$respVar],
         xlab = modelIn$modList$timeVar,ylab = modelIn$modList$respVar)
    plot(modelIn$modList$varIn[,modelIn$modList$timeVar],
         nullResids,
         xlab = modelIn$modList$timeVar,ylab = paste(modelIn$modList$respVar,'mean-centered residuals'),
         sub=paste0('Median absolute error = ',signif(median(abs(nullResids),na.rm=T),3),'; correlation [rho] with ',
                    modelIn$modList$timeVar,': ',
                   round(modelIn$modell$conditional_independence$rawSpearman,3)))
    plot(modelIn$modList$varIn[,modelIn$modList$timeVar],modelResids,
         xlab = modelIn$modList$timeVar,ylab = paste(modelIn$modList$respVar,'model residuals'),
         sub=paste('Median absolute error =',signif(median(abs(modelResids),na.rm=T),3),'; correlation [rho] with ',
                   modelIn$modList$timeVar,': ',
                   round(modelIn$modell$conditional_independence$modelConditionalSpearman,3)))
    par(mfrow=c(1,1))
  }else{
  return(modelResids)
  }

}
