
#' Simulate from a TEfit, assuming multivariate gaussian parameters
#'
#' Requires the $psych$ and $MASS$ packages.
#'
#' @param model_in A TEfit model
#' @param nsim number of simulations to generate
#' @param newdata If desired, new data from which to generate values
#' @param toPlot Logical, for plotting of values.
#'
#' @export
#'
simulate.TEfit <- function(model_in,nsim=100,newdata=data.frame(),toPlot=F){
  if(exists('bootList',model_in)){
  if(dim(newdata)[1]==0){
    newdata <- model_in$modList$varIn
  }else

    if(any(colnames(newdata) != colnames(model_in$modList$varIn))){
      cat('Your input data variables do not match your model variables.\n')
      # break
    }

require(psych)
require(MASS)

  if(toPlot){
    if(!exists('fitThresh',model_in$model)){
    plot(model_in$modList$varIn[,2],model_in$modList$varIn[,1],col='darkred',
         xlab=colnames(model_in$modList$varIn)[2],ylab=colnames(model_in$modList$varIn)[1],
         main='Predictions from models\nsimulated from fit parameter distributions')
    lines(model_in$modList$varIn[,2],model_in$model$fitVals)
    }else{
      plot(model_in$modList$varIn[,2],model_in$model$fitThresh,'l',col='darkred',
           xlab=colnames(model_in$modList$varIn)[2],ylab=colnames(model_in$modList$varIn)[1],
           main='Predictions from models\nsimulated from fit parameter distributions')
    }
  }

parCovar <- cor2cov(
model_in$bootList$bootCorrel[1:length(model_in$modList$pNames),1:length(model_in$modList$pNames)]
,apply(model_in$bootList$bootFits[,1:length(model_in$modList$pNames)],2,sd)
)

newPars <- data.frame(matrix(NA,nsim,dim(parCovar)[2]))
colnames(newPars) <- colnames(parCovar)
newDat <-  data.frame(matrix(NA,nsim,dim(newdata)[1]))
colnames(newDat) <-sprintf('%s%03d','t',1:dim(newdata)[1])

for (curLine in 1:nsim){
newPars[curLine,] <- mvrnorm(1,
                  apply(model_in$bootList$bootFits[,1:length(model_in$modList$pNames)],2,mean)
                  ,parCovar)

parsIn <- as.data.frame(matrix(unlist(newPars[curLine,]),
                            nrow(newdata),length(newPars[curLine,]),byrow=T))
colnames(parsIn) <- model_in$modList$pNames
newDat[curLine,] <- eval(expr=model_in$modList$evalFun,env=data.frame(newdata,parsIn))

if(toPlot){
  if(!exists('fitThresh',model_in$model)){plotY <- newDat[curLine,]}else{
    plotY <- eval(expr=as.formula(paste('~',model_in$modList$thresh_fun
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

