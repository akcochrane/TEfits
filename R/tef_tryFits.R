#' Fit a TE model
#'
#' \code{\link{TEfit}} internal
#'
#' Takes a modList, formats inputs for tef_fitErr(),
#' and uses optim() to find many fits of the modList. Then all of the
#' fits are compared in order to determine which are best and whether the
#' 10 best fits have similar parameters (SD<convergeTol).
#'
#' @param modList List of TEfit model details
#' @param whichPnames Used to distinguish between the TE and null models
#' @param whichFun Used to distinguish between the TE and null models.
#'
#' @export
#'
tef_tryFits <- function(modList,whichPnames='pNames',whichFun='evalFun'){

  if(modList$linkFun$link=='weibull'){linkFunX=modList$linkFun$weibullX}else{linkFunX=NA}
  if(modList$linkFun$link=='logit'){linkFunX=modList$linkFun$logistX}
  if(exists('thresh_fun',modList) && whichFun =='evalFun'){
    thresh_fun <- modList$thresh_fun}else{thresh_fun <- NA} # should bound for null too?

  if (whichPnames=='null_pNames'){paramTerms <- as.list(modList$null_pNames)
  names(paramTerms) <- paramTerms
  penalizeMean <- F
  nullYhat <- 0
  }else{
    nullYhat <- modList$nullYhat
    penalizeMean <- modList$penalizeMean
    paramTerms <- modList$covarTerms}

  modList <- tef_getBounds(modList=modList,whichPnames=whichPnames,linkFunX=linkFunX)

  #  cat(paste(unlist(modList$parGuessBounds),collapse = ' -- '),'\n')

  replTry <- function(modList=modList){

    guesses <- runif(length(modList$guessNames),
                     modList$parGuessBounds$parMin,
                     modList$parGuessBounds$parMax)


    guesses[grep('sigma_param',modList$guessNames)] <- runif(length(grep('sigma_param',modList$guessNames)),
                                                             0,mad(modList$varIn[,modList$respVar],na.rm=T))

    guesses[grep('mu_param',modList$guessNames)] <- runif(length(grep('mu_param',modList$guessNames)),
                                                          0,min(modList$varIn[,modList$respVar],na.rm=T))
    guesses[grep('tau_param',modList$guessNames)] <- runif(length(grep('tau_param',modList$guessNames)),
                                                           0,min(modList$varIn[,modList$respVar],na.rm=T))

    names(guesses) <- modList$guessNames
    {

      tryCatch({
        curFit <- optim(guesses,fn=tef_fitErr,
                        varIn=modList$varIn,pNames=modList$guessNames,evalFun=modList[[whichFun]],
                        errFun=modList$errFun,respVar=modList$respVar,linkFunX=linkFunX,
                        y_lim=modList$y_lim,rate_lim=modList$rate_lim,
                        shape_lim=modList$shape_lim,
                        penalizeMean = c(penalizeMean,mean(nullYhat,na.rm=T)),
                        penalizeRate = modList$penalizeRate,
                        parLims=modList$parLims,
                        thresh_fun = thresh_fun,
                        paramTerms = paramTerms,
                        upper = modList$parLims$parMax,
                        lower = modList$parLims$parMin,
                        method='L-BFGS-B',
                        control=list(maxit=100)
        )
      },error = function(.){try({
        if(modList$quietErrs){cat('\nL-BFGS-B failed')}
        curFit <- optim(guesses,fn=tef_fitErr,
                        varIn=modList$varIn,pNames=modList$guessNames,evalFun=modList[[whichFun]],
                        errFun=modList$errFun,respVar=modList$respVar,linkFunX=linkFunX,
                        y_lim=modList$y_lim,rate_lim=modList$rate_lim,
                        shape_lim=modList$shape_lim,
                        penalizeMean = c(penalizeMean,mean(nullYhat,na.rm=T)),
                        penalizeRate = modList$penalizeRate,
                        parLims=modList$parLims,
                        thresh_fun = thresh_fun,
                        paramTerms = paramTerms,
                        method='BFGS',  # use this or L-BFGS-B (with upper and lower) if bounds have been figured out.
                        control=list(maxit=100)
        )
      },silent=T)
      })

    }
    if(exists('curFit')){
      return(c(err=curFit$value,curFit$par))
    }else{
      return(c(err=1E20,guesses))
    }

  }

  converged <- F
  sumTries <- 0
  bestFits <- data.frame()
  nPerRep <- 10

  while (converged==F && sumTries < modList$nTries){

    allFit <- replicate(nPerRep,replTry(modList))

    bestFits <- rbind(bestFits,
                      data.frame(t(allFit)))

    bestFits <- bestFits[order(bestFits[,1]),][1:max(10,nPerRep),]

    ## ## with several parameters
    try({
      if(ncol(bestFits)>2){
        if (
          max(apply(bestFits[,2:ncol(bestFits)],2,sd)) < modList$convergeTol
          &&
          max(bestFits[1,2:ncol(bestFits)]-colMeans(bestFits[2:nrow(bestFits),2:ncol(bestFits)])) < modList$convergeTol
        ){
          converged=T
        }
      }else if( ## with only time as predictor
        max(sd(bestFits[,2])) < modList$convergeTol
        &&
        max(bestFits[1,2]-mean(bestFits[2:10,2])) < modList$convergeTol
      ){
        converged=T
      }
    },silent=T)

    sumTries <- sumTries+nPerRep
  }



  if(!converged && !modList$suppressWarnings){cat('\nWarning: model did not converge at tol =',modList$convergeTol,'. Consider respecifying, allowing more runs, or increasing the convergence tolerance.\n')}

  bestFit <- as.numeric(bestFits[1,])

  # ## ##
  # ensure that the reported error is the actual [unpenalized] error
  bestFit[1] <- tef_fitErr(bestFit[2:length(bestFit)],
                           varIn=modList$varIn,pNames=modList$guessNames,evalFun=modList[[whichFun]],
                           errFun=modList$errFun,respVar=modList$respVar,linkFunX=linkFunX,
                           y_lim=modList$y_lim,rate_lim=modList$rate_lim,
                           shape_lim=modList$shape_lim,
                           penalizeMean = c(F,mean(nullYhat,na.rm=T)),
                           penalizeRate = F,
                           parLims=modList$parLims,
                           thresh_fun = thresh_fun,
                           paramTerms = paramTerms)
  ## ## ##


  names(bestFit) <- c('err',modList[[whichPnames]])

  ## throw a warning if the rate is within 1% of the limits
  if(!modList$penalizeRate && !modList$suppressWarnings){

      if(!is.na(bestFit['pRate']) ){
        if(
          (bestFit['pRate']-modList$rate_lim[1])/(modList$rate_lim[2]-modList$rate_lim[1])<.01 ||
          (bestFit['pRate']-modList$rate_lim[1])/(modList$rate_lim[2]-modList$rate_lim[1])>.99
        ){cat('\nYour rate is very close to the boundary. Consider penalizing the likelihood.')}}

      if(!is.na(bestFit['pRate_0']) ){
        if(
          (bestFit['pRate_0']-modList$rate_lim[1])/(modList$rate_lim[2]-modList$rate_lim[1])<.01 ||
          (bestFit['pRate_0']-modList$rate_lim[1])/(modList$rate_lim[2]-modList$rate_lim[1])>.99
        ){cat('\nYour rate is very close to the boundary. Consider penalizing the likelihood.')}}

    }




  bestFit <- list(value=bestFit[1],par=bestFit[2:length(bestFit)],converged=converged,runs = sumTries)

  return(bestFit)
}
