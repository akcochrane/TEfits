
#' Simulate from a TEfit, assuming multivariate gaussian parameters
#'
#' Requires the $psych$ and $MASS$ packages.
#'
#' @param object A TEfit model
#' @param nsim number of simulations to generate
#' @param newdata If desired, new data from which to generate values
#' @param toPlot Logical, for plotting of values.
#' @param seed Null, to retain symmetry with other simulate() methods
#'
#' @method simulate TEfit
#' @export
#'
#' @examples
#' \dontrun{simulate(model_fit_by_TEfit)}
#'
simulate.TEfit <- function(object,nsim=100,seed=NULL,newdata=data.frame(),toPlot=F){

require(psych)
require(MASS)

  if(exists('bootList',object)){
  if(dim(newdata)[1]==0){
    newdata <- object$modList$varIn
  }else

    if(any(colnames(newdata) != colnames(object$modList$varIn))){
      cat('Your input data variables do not match your model variables.\n')
      # break
    }

  if(toPlot){
    if(!exists('fitThresh',object$model)){
    plot(object$modList$varIn[,2],object$modList$varIn[,1],col='darkred',
         xlab=colnames(object$modList$varIn)[2],ylab=colnames(object$modList$varIn)[1],
         main='Predictions from models\nsimulated from fit parameter distributions')
    lines(object$modList$varIn[,2],object$model$fitVals)
    }else{
      plot(object$modList$varIn[,2],object$model$fitThresh,'l',col='darkred',
           xlab=colnames(object$modList$varIn)[2],ylab=colnames(object$modList$varIn)[1],
           main='Predictions from models\nsimulated from fit parameter distributions')
    }
  }

parCovar <- cor2cov(
object$bootList$bootCorrel[1:length(object$modList$pNames),1:length(object$modList$pNames)]
,apply(object$bootList$bootFits[,1:length(object$modList$pNames)],2,sd)
)

newPars <- data.frame(matrix(NA,nsim,dim(parCovar)[2]))
colnames(newPars) <- colnames(parCovar)
newDat <-  data.frame(matrix(NA,nsim,dim(newdata)[1]))
colnames(newDat) <-sprintf('%s%03d','t',1:dim(newdata)[1])

for (curLine in 1:nsim){
newPars[curLine,] <- mvrnorm(1,
                  apply(object$bootList$bootFits[,1:length(object$modList$pNames)],2,mean)
                  ,parCovar)

parsIn <- as.data.frame(matrix(unlist(newPars[curLine,]),
                            nrow(newdata),length(newPars[curLine,]),byrow=T))
colnames(parsIn) <- object$modList$pNames
newDat[curLine,] <- eval(expr=object$modList$evalFun,env=data.frame(newdata,parsIn))

if(toPlot){
  if(!exists('fitThresh',object$model)){plotY <- newDat[curLine,]}else{
    plotY <- eval(expr=as.formula(paste('~',object$modList$thresh_fun
                                        ))[[2]],env=data.frame(newdata,parsIn))
      }
lines(newdata[,2],plotY,col=rgb(.3,.3,.8,max((1/nsim),.005)),lwd=5)
}


}
return(list(newPars=newPars,newDat=newDat))
  }else{
  cat('\nYour model does not contain any bootstrapped fits.\n')
  }
}

