#' Generalized linear mixed-effects model with nonlinear time random effects
#'
#' Fits a \code{\link[lme4]{glmer}} generalized linear mixed-effects model with the random effects of
#' \code{timeVar} for each level of \code{groupingVar}. Provides estimates of time-related change
#' (i.e., attempts to answer the question "how different was the start than the end?").
#'
#' First uses \code{\link{TEglm}} to find a rate parameter for each level of \code{groupingVar}. These
#' rate parameters are used to transform the corresponding \code{timeVar} into a exponentially
#' saturating variable (see \code{\link{TEglm}}). After finding bivariate rate parameters using \code{\link{TEglm}},
#' \code{TEglmem} attempts to optimize the vector of rate parameters in conjunction with the full
#' \code{glmer} model.
#'
#' May be used, with \code{onlyGroupMods=T}, as a wrapper for \code{\link{TEglm}} in order to simply
#' fit bivariate \code{response~time} models and extract the corresponding transformed time variable.
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
#' In \code{\link{TEfit}} and \code{\link{TEfitAll}} rate [50 percent time constant] is binary-log-transformed.
#' Here it is not.
#'
#'
#' @param formIn model formula, as in \code{glmer}
#' @param dat model data, as in \code{glmer}
#' @param groupingVar String. Indicates which variable in \code{datIn} should have a time=related random effect.
#' @param family model family, as in \code{glmer}
#' @param onlyGroupMods IF TRUE, returns only the by-\code{groupingVar} fits from \code{TElm}
#' @inheritParams TElmem
#'
#' @return
#' A list including:
#' \describe{
#' \item{\code{glmerMod}}{\code{\link[lme4]{glmer}} model fit with transformed time variable}
#' \item{\code{rates}}{Named vector of rates [\emph{50-percent-of-change time constants}]}
#' \item{\code{timeDat}}{Data frame with original and transformed time variable}
#' \item{\code{groupMods}}{List of fit \code{\link{TEglm}} models, and the corresponding transformed time variable and named vector of rates}
#' }
#'
#' @export
#'
TEglmem <- function(formIn,dat,timeVar,groupingVar,family=gaussian,onlyGroupMods=F,nRuns = 5){
  require(lme4)
  origTime <-   dat[,timeVar]

  rateBounds <- quantile(dat[,timeVar],c(1/30,1/3))

  groupNames <- unique(dat[,groupingVar])
  nGroups <- length(groupNames)


  # first estimate bivariate rates
  {
    groupMods <- list() ; rateVect <- c()
    timeDat <- data.frame(groupName=c(),original=c(),transformed=c()) ; for(curGroupName in groupNames){
      groupMods[[curGroupName]] <- TEglm(as.formula(paste(as.formula(formIn)[[2]],'~',timeVar)),
                                        timeVar = timeVar,
                                        datIn = dat[dat[,groupingVar]==curGroupName,],
                                        family=family
      )
      rateVect[curGroupName] <- groupMods[[curGroupName]]$rate
      timeDat <- rbind(timeDat,data.frame(
        groupName=curGroupName,
        original=dat[dat[,groupingVar]==curGroupName,timeVar],
        transformed=groupMods[[curGroupName]]$transformed_time
      ))
    }
    if(onlyGroupMods){return(list(models=groupMods,rates=rateVect,timeDat=timeDat))}
    # rm(curGroupName,timeDat)
  }

  ## initialize a model with the by-group fits:
  {
    curDat <- dat
    bestRates <- rateVect
    for(curGV in groupNames){
      curDat[curDat[,groupingVar]==curGV,timeVar] <-
        2^((1-curDat[curDat[,groupingVar]==curGV,timeVar])/bestRates[curGV])
    }
    initMod <- glmer(formIn,curDat,family=family)
    bestNegLL <- -logLik(initMod)
    rm(curGV,curDat)
  }


  # optimize directly
  {
  fitFun <- function(rates,curDat,formula,timeVar,groupNames,rateBounds){
    for(curGV in groupNames){
      curDat[curDat[,groupingVar]==curGV,timeVar] <-
        2^((1-curDat[curDat[,groupingVar]==curGV,timeVar])/rates[curGV])
    }
    modNegLL <- 1E15

    if(any(rates < rateBounds[1]) || any(rates > rateBounds[2]) ){
      modNegLL <- 1E15
    }else{try({
      suppressMessages({ suppressWarnings({
        curMod <- glmer(formula,curDat,family=family)
        modNegLL <- -logLik(curMod)
      })})},silent=T)
    }

    return(modNegLL)
  }
  cat('[')
  bestNegLL <- 1E15; for(. in 1:nRuns){
    rates <- (rateVect+runif(nGroups,rateBounds[1],rateBounds[2]))/2
    names(rates) <- groupNames
    mF <- optim(rates,
                fn=fitFun,
                curDat=dat,
                formula=formIn,
                timeVar=timeVar,
                groupNames=groupNames,
                rateBounds=rateBounds)
    cat('=')
    if(mF$value < bestNegLL){
      bestRates <- mF$par
      names(bestRates) <- groupNames
      bestNegLL <- mF$value
      }
  }

} # close optimizing directly

  for(curGV in groupNames){
    dat[dat[,groupingVar]==curGV,timeVar] <-
      2^((1-dat[dat[,groupingVar]==curGV,timeVar])/bestRates[curGV])
  }

  outMod <- glmer(formIn,dat,family=family)
    cat(']')
  return(list(glmerMod=outMod,
              rates=bestRates,
              groupMods=groupMods,
              timeDat=data.frame(original=origTime,transformed=dat[,timeVar])))
}

