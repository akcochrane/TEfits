#' Fit a model to resampled data
#'
#' {{RE TEST}} Given the modList constructed in TEfit(), refit the model nBoots number of times
#' on bootPercent proportion of the data. If bootPercent is 1, sampling is done with replacement.
#' If bootPercent is between 0 and 1, sampling is done without replacement, i.e., a random
#' subset of the data is selected. This latter case allows train/test assessment. That is,
#' the model is fit to some subset of the data, after which the predicted values for the remaining data
#' are calculated. The error of these out-of-sample predictions are recorded alongside the
#' in-sample goodness-of-fit.
#'
#' Number of runs per resample [bootTries] defaults to 10, the minimum.
#'
#' @param modList List of TEfit model details
#'
#' @export
#'
tef_bootFits <- function(modList){

  if(!exists('bootPercent',modList$bootPars)){
    modList$bootPars$bootPercent <- 1
  }
  if(!exists('bootTries',modList$bootPars)){
    modList$bootPars$bootTries <- 10
  }

  bootList <- list()
  bootList$bootFits <- data.frame()
  bootList$bootPreds <- data.frame(matrix(NA,modList$bootPars$nBoots,dim(modList$varIn)[1]))
  colnames(bootList$bootPreds) <- sprintf('t%02d',modList$varIn[,modList$timeVar])

  bootErr <- F

  for(curBoot in 1:modList$bootPars$nBoots){

    if (bootErr){cat('A resample fit failed. ')}
    bootErr <- T

    shuffleInds <- sample(dim(modList$varIn)[1],
                          replace = (modList$bootPars$bootPercent==1))

    trainInds <- shuffleInds[1:round(modList$bootPars$bootPercent*dim(modList$varIn)[1])]

    if(modList$bootPars$bootPercent<1){
    transInds <- shuffleInds[(round(modList$bootPars$bootPercent*dim(modList$varIn)[1])+1):dim(modList$varIn)[1]]
    }

    bootFitList <- modList
    bootFitList$suppressWarnings <- T
    bootFitList$varIn <- bootFitList$varIn[trainInds,]
    bootFitList$nTries <- modList$bootPars$bootTries

    try({
    bootFit <- tef_tryFits(bootFitList)  ## fit the resampled model

    bootFitted <- as.data.frame(matrix(bootFit$par,dim(modList$varIn)[1],length(bootFit$par),byrow=T))
    colnames(bootFitted) <- modList$pNames

    if(modList$bootPars$bootPercent<1){ ## special things, for subsampling
    oosDat <- as.data.frame(matrix(bootFit$par,length(transInds),length(bootFit$par),byrow=T))
    colnames(oosDat) <- modList$pNames
    oosDat <- data.frame(modList$varIn[transInds,],oosDat)

    oosFit <- eval(expr=modList$evalFun,env=oosDat)


    ### NEED TO MODULARIZE ERROR HERE, AND IN FITERR
    err_oos <- tef_err(y=oosDat[,modList$respVar],yHat=oosFit,errFun=modList$errFun)
    if(modList$errFun != 'rmse'){
    err_oos_mean <- err_oos/dim(oosDat)[1]
    }else{err_oos_mean <- err_oos}

    bootList$bootFits <- rbind(bootList$bootFits,
                              data.frame(t(bootFit$par),
                                         err=bootFit$value,
                                         err_mean =bootFit$value/length(trainInds),
                                         err_oos = err_oos,
                                         err_oos_mean = err_oos_mean,
                                         err_type = modList$errFun
                              )
                              )

    rm(oosFit,transInds,oosDat,err_oos,err_oos_mean)
    }else{
      bootList$bootFits <- rbind(bootList$bootFits,
                                data.frame(t(bootFit$par),
                                           err=bootFit$value,
                                           err_mean =bootFit$value/length(trainInds),
                                           err_oos = NA,
                                           err_oos_mean = NA,
                                           err_type = modList$errFun
                                )
      )
    }

    bootList$bootPreds[curBoot,] <-
      eval(expr=modList$evalFun,env=data.frame(modList$varIn,bootFitted))

    bootErr <- F

    rm(bootFitList,shuffleInds,trainInds)
    },silent=T)
  }

  ## find the trial at which predicted values diverge from initial values with d>=1
  try({
    curD <- 0
    curT <- 1
    while(abs(curD)<1 && !is.na(curD)){
      curT <- curT+1

      curDat <- data.frame(
        x=c(bootList$bootPreds[,1],bootList$bootPreds[,curT]),
        group = c(rep(0,modList$bootPars$nBoots),rep(1,modList$bootPars$nBoots))
      )

      curD <-
  (mean(curDat[curDat$group==1,'x']) - mean(curDat[curDat$group==0,'x']))/
        mean(c(sd(curDat[curDat$group==1,'x']),sd(curDat[curDat$group==0,'x'])))



      if(curT==dim(bootList$bootPreds)[2]){curT <- NA}
    }
  },silent=T)
  bootList$time_d_over_1 <- modList$varIn[curT,modList$timeVar][1]


  ending_predicted_values <- bootList$bootPreds[,dim(bootList$bootPreds)[2]]
  beginning_predicted_values <- bootList$bootPreds[,1]

  curTest  <- wilcox.test(ending_predicted_values,
                          beginning_predicted_values,
                                        paired=T)
  medDiff <- median(ending_predicted_values) - median(beginning_predicted_values)
  bootList$slope_nonzero_nonparametric <- data.frame(z = qnorm(curTest$p.value/2) * sign(-medDiff),
                    p = curTest$p.value, w = curTest$statistic, medDiff = medDiff)
  rownames(bootList$slope_nonzero_nonparametric) <- 'Wilcoxon Signed-Rank Test: '

  bootList$slope_nonzero_D <-
    (mean(ending_predicted_values) - mean(beginning_predicted_values))/
    mean(c(sd(ending_predicted_values),sd(beginning_predicted_values)))

  bootList$percent_increasing <- mean((ending_predicted_values-beginning_predicted_values)>0)

  bootList$boots <- apply(bootList$bootFits[,1:(dim(bootList$bootFits)[2]-1)],2,quantile,
                         c(.01,.025,.1,pnorm(-1),.25,.5,.75,pnorm(1),.9,.975,.99),na.rm=T)
  bootList$bootCorrel <- cor(bootList$bootFits[,1:(dim(bootList$bootFits)[2]-1)])
  bootList$nBoots <- modList$bootPars$nBoots
  bootList$bootPercent <- modList$bootPars$bootPercent
  return(bootList)
}
