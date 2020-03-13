#' Plot a TEfit
#'
#' Plot predicted values of a TEfit model. Predictions are thresholds, if relevant;
#' otherwise they are overall model predictions
#'
#' @param TEs3  TEfit model output
#' @param plot_title  optional plot title
#' @param xlabel optional plot x axis label
#' @param ylabel optional plot y axis label
#' @param sub_title optional plot caption
#' @param ymin optional lower boundary for Y axis
#' @param ymax optional upper boundary for X axis
#'
#'
#' @export
#'
#' @examples
#' \dontrun{plot(model_fit_by_TEfit)}
#'
plot.TEfit <- function(TEs3,plot_title='',xlabel='',ylabel='',sub_title='',ymin=NA,ymax=NA){

  if(nchar(plot_title)==0){
    plot_title <- paste('Time-evolving function:',TEs3$modList$changeFun)
  }
  if(nchar(xlabel)==0){
    xlabel=TEs3$modList$timeVar
  }
  if(nchar(ylabel)==0){
    ylabel <- TEs3$modList$respVar
  }
  if(nchar(sub_title)==0){
    sub_title <- 'Blue line is fit controlling for covariates. Black line is fit values of the full model.'
  }
  if(is.na(ymin)){
    ymin=min(TEs3$modList$varIn[,TEs3$modList$respVar],na.rm=T)
  }
  if(is.na(ymax)){
    ymax=max(TEs3$modList$varIn[,TEs3$modList$respVar],na.rm=T)
  }

  par(family='serif')
  if(TEs3$modList$linkFun$link=='identity' || TEs3$modList$linkFun$link=='d_prime'){
    plot(sort(TEs3$modList$varIn[,TEs3$modList$timeVar]),
         TEs3$modList$varIn[order(TEs3$modList$varIn[,TEs3$modList$timeVar]),TEs3$modList$respVar],
         col='darkred',ylab=ylabel,xlab=xlabel,
         main=plot_title,
         ylim=c(ymin,ymax))
    lines(sort(TEs3$modList$varIn[,TEs3$modList$timeVar]),
          TEs3$model$fitVals[order(TEs3$modList$varIn[,TEs3$modList$timeVar])],lwd=2)
    if(dim(TEs3$modList$varIn)[2]>(as.numeric(TEs3$modList$linkFun$link=='d_prime')+2)){
      lines(TEs3$modList$varIn[,TEs3$modList$timeVar],TEs3$model$fitVals_noCovar,lwd=2,col='darkblue')
      mtext(sub_title)
    }

    if(TEs3$modList$bootPars$nBoots > 0){
      lines(TEs3$modList$varIn[,TEs3$modList$timeVar],
            apply(TEs3$bootList$bootPred,2,quantile,.975,na.rm=T),col='lightblue')
      lines(TEs3$modList$varIn[,TEs3$modList$timeVar],
            apply(TEs3$bootList$bootPred,2,quantile,.025,na.rm=T),col='lightblue')
    }
  }

  if(TEs3$modList$linkFun$link=='logit' || TEs3$modList$linkFun$link=='weibull'){
    plot(TEs3$modList$varIn[,TEs3$modList$timeVar],TEs3$model$fitThresh,'l',
         ylab='Threshold',xlab=xlabel,main=plot_title)
    if(TEs3$modList$bootPars$nBoots > 0){
      lines(TEs3$modList$varIn[,TEs3$modList$timeVar],TEs3$bootList$threshCI$ci025,col='lightblue')
      lines(TEs3$modList$varIn[,TEs3$modList$timeVar],TEs3$bootList$threshCI$ci975,col='lightblue')
    }

  }

}
