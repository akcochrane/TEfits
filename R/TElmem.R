
#' Linear mixed-effects model with nonlinear time random effects
#'
#' Fits a \code{\link[lme4]{lmer}} linear mixed-effects model with the random effects of
#' \code{timeVar} for each level of \code{groupingVar}. Provides estimates of time-related change
#' (i.e., attempts to answer the question "how different was the start than the end?").
#'
#' First uses \code{\link{TElm}} to find a rate parameter for each level of \code{groupingVar}, with
#' the formula extracted using \code{\link{tef_getRanefForm}}. These
#' rate parameters are used to transform the corresponding \code{timeVar} into a exponentially
#' saturating variable (see \code{\link{TElm}}). After finding an initial set of
#' rate parameters using \code{\link{TElm}},
#' \code{TElmem} attempts to optimize the vector of rate parameters in conjunction with the full
#' \code{lmer} model.
#'
#' May be used, with \code{nRuns=0}, as a wrapper for \code{\link{TElm}} in order to simply
#' use rate estimates from independent \code{groupingVar}-level models, extract the corresponding
#' transformed time variable, and use this in a LMEM.
#'
#' @note
#' Random effects and rate estimates may be unstable, and optimization may take
#' a very long time to run. The primary purpose of this function is to allow for by-\code{groupingVar}
#' detrending of time-related changes in data (i.e., to estimate and test fixed effects at asymptotic time,
#' or to estimate and test the magnitude of time-related effects).
#' If reliable by-\code{groupingVar} parameters are desired, especially of rate, it is highly recommended to use
#' \code{\link{TEfit}} or \code{\link{TEfitAll}}.
#'
#' The \code{formIn} must include a random effect of \code{timeVar} by \code{groupingVar}
#' (e.g., \code{(time_variable | grouping_variable)})
#'
#'
#' @param formIn model formula, as in \code{lmer}
#' @param dat model data, as in \code{lmer}
#' @param timeVar String. Indicates which variable in \code{datIn} corresponds to time (i.e., should be transformed). Must be numeric and positive.
#' @param groupingVar String. Indicates which variable in \code{datIn} should have a time=related random effect.
#' @param nRuns Number of times to run optimization of the rate (i.e., fitting nonlinear transformations of \code{timeVar})
#' @param startingOffset By default (if T) time is coded to start at 1 and saturate to 0. If startingOffset is F, time starts at 0 and saturates to 1. May assist in interpreting interactions with other variables, etc.
#' @param silent Progress is printed by default. silent=T to suppress
#'
#' @return
#' A list including:
#' \describe{
#' \item{\code{lmerMod}}{\code{\link[lme4]{lmer}} model fit with transformed time variable}
#' \item{\code{rates}}{Named vector of rates [\emph{50-percent-of-change time constants}]}
#' \item{\code{timeDat}}{Data frame with original and transformed time variable}
#' \item{\code{groupMods}}{List of fit \code{\link{TElm}} models, and the corresponding transformed time variable and named vector of rates}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' m_TElmem <- TElmem(acc ~ trialNum + (trialNum || subID), anstrain, timeVar = 'trialNum',groupingVar = 'subID')
#' # Typical lmer model:
#' summary(m_TElmem$lmerMod) # On average, starting accuracy was .137 worse than asymptotic accuracy
#' # Participant-level rate parameters:
#' m_TElmem$rates
#' }
TElmem <- function(formIn,dat,timeVar,groupingVar,nRuns = 1,startingOffset=T,silent=F){
  require(lme4)

  minTime <- min(dat[,timeVar],na.rm = T)
  if(minTime < 0){cat('The earliest time is negative.')}

  formIn <- as.formula(formIn)

  origTime <-   dat[,timeVar]

  rateBounds <- quantile(log2(dat[,timeVar]),c(1/30,1/3))

  groupNames <- unique(dat[,groupingVar])
  nGroups <- length(groupNames)

  if(!silent){cat('[')}
  # first estimate group-level rates
  {
    groupForm <- tef_getRanefForm(formIn,groupingVar)
    groupMods <- list() ; rateVect <- c()
    timeDat <- data.frame(groupName=c(),original=c(),transformed=c()) ; for(curGroupName in groupNames){
      groupMods[[curGroupName]] <- TElm(groupForm,
                                        timeVar = timeVar,
                                        dat = dat[dat[,groupingVar]==curGroupName,],
                                        startingOffset = startingOffset,
                                        nBoot = 20
      )
      rateVect[curGroupName] <- groupMods[[curGroupName]]$rate
      timeDat <- rbind(timeDat,data.frame(
        groupName=curGroupName,
        original=dat[dat[,groupingVar]==curGroupName,timeVar],
        transformed=groupMods[[curGroupName]]$transformed_time
      ))
    }

    rm(curGroupName)
  }

  ## initialize a model with the by-group fits:
  {
    curDat <- dat
    bestRates <- rateVect
    for(curGV in groupNames){
      curDat[curDat[,groupingVar]==curGV,timeVar] <-
        2^((minTime-curDat[curDat[,groupingVar]==curGV,timeVar])/(2^bestRates[curGV]))
      if(!startingOffset){curDat[curDat[,groupingVar]==curGV,timeVar] <- 1-curDat[curDat[,groupingVar]==curGV,timeVar]}
    }
    initMod <- lmer(formIn,curDat)
    bestNegLL <- -logLik(initMod)
    rm(curGV,curDat)
  }
  if(!silent){cat('=')}

  if(nRuns==0){cat(']\n') ; return(list(models=groupMods,rates=rateVect,timeDat=timeDat,lmerMod = initMod))}

  ## define function to optimize (input a vector of rates to minimize negative L)
  fitFun <- function(rates,curDat,formula,timeVar,groupNames,rateBounds,startingOffset){
    for(curGV in groupNames){
      curDat[curDat[,groupingVar]==curGV,timeVar] <-
        2^((minTime-curDat[curDat[,groupingVar]==curGV,timeVar])/(2^rates[curGV]))
      if(!startingOffset){curDat[curDat[,groupingVar]==curGV,timeVar] <- 1-curDat[curDat[,groupingVar]==curGV,timeVar]}
    }
    modNegLL <- 1E15
    try({
      suppressMessages({ suppressWarnings({
        curMod <-  lmer(formula,curDat)
        modNegLL <- -logLik(curMod)
      })})},silent=F)

    if(any(rates < rateBounds[1]) || any(rates > rateBounds[2]) ){
      modNegLL <- 1E15 # penalize for being outside of boundaries
    }

    return(modNegLL)
  }

  ## run optimization nRuns times

  for(. in 1:nRuns){
    rates <- (rateVect*4+runif(nGroups,rateBounds[1],rateBounds[2]))/5 # try random starting points for optimization, biased toward group-fit rates
    names(rates) <- groupNames
    mF <- optim(rates,
                fn=fitFun,
                curDat=dat,
                formula=formIn,
                timeVar=timeVar,
                groupNames=groupNames,
                rateBounds=rateBounds,
                startingOffset=startingOffset)
    if(!silent){cat('=')}
    if(mF$value < bestNegLL){
      bestRates <- mF$par
      names(bestRates) <- groupNames
      bestNegLL <- mF$value
    }
  }

  for(curGV in groupNames){
    dat[dat[,groupingVar]==curGV,timeVar] <-
      2^((minTime-dat[dat[,groupingVar]==curGV,timeVar])/(2^bestRates[curGV]))
    if(!startingOffset){dat[dat[,groupingVar]==curGV,timeVar] <- 1-dat[dat[,groupingVar]==curGV,timeVar]}
  } ; rm(curGV)

  outMod <- lmer(formIn,dat)
  if(!silent){cat(']\n')}
  return(list(lmerMod=outMod,
              rates=bestRates,
              groupMods=list(models=groupMods,rates=rateVect),
              timeDat=data.frame(original=origTime,transformed=dat[,timeVar])))
}
