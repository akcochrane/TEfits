#' Generalized linear mixed-effects model with nonlinear time random effects
#'
#' Fits a \code{\link[lme4]{glmer}} generalized linear mixed-effects model with the random effects of
#' \code{timeVar} for each level of \code{groupingVar}. Provides estimates of time-related change
#' (i.e., attempts to answer the question "how different was the start than the end?").
#'
#' First uses \code{\link{TEglm}} to find a rate parameter for each level of \code{groupingVar}, with
#' the formula extracted using \code{\link{tef_getRanefForm}}. These
#' rate parameters are used to transform the corresponding \code{timeVar} into a exponentially
#' saturating variable (see \code{\link{TEglm}}). After finding an initial set of
#' rate parameters using \code{\link{TEglm}},
#' \code{TEglmem} attempts to optimize the vector of rate parameters in conjunction with the full
#' \code{glmer} model.
#'
#' May be used, with \code{nRuns=0}, to simply
#' use rate estimates from independent \code{groupingVar}-level \code{\link{TEglm}} models, extracting the corresponding
#' transformed time variables and using them in a GLMEM.
#'
#' @note
#' Random effects and rate estimates may be unstable, and optimization may take
#' a very long time to run. The primary purpose of this function is to allow for by-\code{groupingVar}
#' detrending of time-related changes in data (i.e., to estimate and test fixed effects at asymptotic time,
#' or to estimate and test the magnitude of time-related effects).
#' If reliable by-\code{groupingVar} parameters are desired, it is highly recommended to use
#' \code{\link{TEfit}} or \code{\link{TEfitAll}}.
#'
#' The \code{formIn} must include a random effect of \code{timeVar} by \code{groupingVar}
#' (e.g., \code{(time_variable | grouping_variable)}).
#'
#' Although the time variable is transformed to exponentially decay from one toward zero
#' (or, if \code{startingOffset=F}, from zero toward one), this does not necessarily mean
#' that the model prediction involves an exponential change with time. The nonlinear change in time relates
#' to the time-associated model coefficients.
#'
#'
#' @param formIn model formula, as in \code{glmer}
#' @param dat model data, as in \code{glmer}
#' @param groupingVar String. Indicates which variable in \code{datIn} should have a time-related random effect.
#' @param family model family, as in \code{glmer}
#' @inheritParams TElmem
#'
#' @return
#' A list including:
#' \describe{
#' \item{\code{glmerMod}}{\code{\link[lme4]{glmer}} model fit with transformed time variable}
#' \item{\code{rates}}{Named vector of rates [\emph{binary log of 50-percent-of-change time constants}]}
#' \item{\code{timeDat}}{Data frame with original and transformed time variable}
#' \item{\code{groupMods}}{List of fit \code{\link{TEglm}} models, and the corresponding transformed time variable and named vector of rates}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' m_TEglmem <- TEglmem(resp ~ ratio + trialNum:ratio + (ratio + trialNum:ratio || subID),anstrain, timeVar = 'trialNum',groupingVar = 'subID',family=binomial)
#' # Typical glmer model:
#' summary(m_TEglmem$glmerMod)
#' # Participant-level rate parameters:
#' m_TEglmem$rates
#' }
#'
TEglmem <- function(formIn,dat,timeVar,groupingVar,family=gaussian,startingOffset=T,nRuns = 1,silent=F){
  require(lme4)
  origTime <-   dat[,timeVar]

  minTime <- min(dat[,timeVar],na.rm = T)
  if(minTime < 0){cat('The earliest time is negative.')}

  rateBounds <- quantile(log2(dat[,timeVar]),c(1/30,1/3))

  groupNames <- unique(dat[,groupingVar])
  nGroups <- length(groupNames)

  if(!silent){cat('[')}
  # first estimate group-level rates
  {
    groupForm <- tef_getRanefForm(formIn,groupingVar)
    groupMods <- list() ; rateVect <- c()
    timeDat <- data.frame(groupName=c(),original=c(),transformed=c()) ; for(curGroupName in groupNames){
      groupMods[[curGroupName]] <- TEglm(groupForm,
                                        timeVar = timeVar,
                                        dat = dat[dat[,groupingVar]==curGroupName,],
                                        family=family,
                                        startingOffset = startingOffset
      )
      rateVect[curGroupName] <- groupMods[[curGroupName]]$rate
      timeDat <- rbind(timeDat,data.frame(
        groupName=curGroupName,
        original=dat[dat[,groupingVar]==curGroupName,timeVar],
        transformed=groupMods[[curGroupName]]$transformed_time
      ))
    }
  }

  ## initialize a model with the by-group fits:
  {
    curDat <- dat
    bestRates <- rateVect ; names(bestRates) <- groupNames
    for(curGV in groupNames){
      curDat[curDat[,groupingVar]==curGV,timeVar] <-
        2^((minTime-curDat[curDat[,groupingVar]==curGV,timeVar])/(2^bestRates[curGV]))
      if(!startingOffset){curDat[curDat[,groupingVar]==curGV,timeVar] <- 1-curDat[curDat[,groupingVar]==curGV,timeVar]}
    }
    suppressMessages({ suppressWarnings({
    initMod <- glmer(formIn,curDat,family=family)
    })})
    bestNegLL <- -logLik(initMod)
    rm(curGV,curDat)
  }
  if(!silent){cat('=')}
 if(nRuns==0){if(!silent){cat(']\n')} ; return(list(models=groupMods,rates=rateVect,timeDat=timeDat,glmerMod = initMod))}
  # optimize directly
  {
  fitFun <- function(rates,curDat,timeVar,groupNames,rateBounds,startingOffset,lme4mod){
    for(curGV in groupNames){
      curDat[curDat[,groupingVar]==curGV,timeVar] <-
        2^((minTime-curDat[curDat[,groupingVar]==curGV,timeVar])/(2^rates[curGV]))
      if(!startingOffset){curDat[curDat[,groupingVar]==curGV,timeVar] <- 1-curDat[curDat[,groupingVar]==curGV,timeVar]}
    }
    modNegLL <- 1E16

    if(any(rates < rateBounds[1]) || any(rates > rateBounds[2]) ){
      modNegLL <- 1E16
    }else{try({
      suppressMessages({ suppressWarnings({
        curMod <- update(lme4mod,curDat)
        modNegLL <- -logLik(curMod)
        rm(curMod)
      })})},silent=T)
    }

    return(modNegLL)
  }

  for(. in 1:nRuns){
    rates <- (rateVect*4+runif(nGroups,rateBounds[1],rateBounds[2]))/5
    names(rates) <- groupNames
    mF <- optim(rates,
                fn=fitFun,
                curDat=dat,
                timeVar=timeVar,
                groupNames=groupNames,
                rateBounds=rateBounds,
                startingOffset=startingOffset,
                lme4mod = initMod,
                method="Nelder-Mead",
                control=list(maxit=50))
    if(!silent){cat('=')}
    if(mF$value < bestNegLL){
      bestRates <- mF$par ;  names(bestRates) <- groupNames
      bestNegLL <- mF$value
      }
  }

} # close optimizing directly

  for(curGV in groupNames){
    dat[dat[,groupingVar]==curGV,timeVar] <-
      2^((minTime-dat[dat[,groupingVar]==curGV,timeVar])/(2^bestRates[curGV]))
    if(!startingOffset){dat[dat[,groupingVar]==curGV,timeVar] <- 1 - dat[dat[,groupingVar]==curGV,timeVar]}
  }

  outMod <- glmer(formIn,dat,family=family)
  if(!silent){cat(']\n')}
  return(list(glmerMod=outMod,
              rates=bestRates,
              groupMods=list(models=groupMods,rates=rateVect),
              timeDat=data.frame(original=origTime,transformed=dat[,timeVar])))
}

