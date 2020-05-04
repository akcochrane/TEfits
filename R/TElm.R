
#' [Robust] linear model with nonlinear time predictor
#'
#' Fit a linear model or robust linear model with time as a covariate,
#' while estimating the shape of the nonlinear interpolation between starting and ending time.
#' First resamples data with replacement 100 times, and each time estimates the best-shaped curve to interpolate
#' between initial time-related offset and asymptotic time (i.e., rate at which effect of time saturates at zero).
#' Then uses the median estimated rate to transform the \code{timeVar} predictor into an exponentially decaying variable
#' interpolating between initial time (time offset magnitude of 1) and arbitrarily large time values (time
#' offset magnitude 0). Last uses this transformed time variable in a \code{\link{tef_rlm_boot}} [\code{rlm} or \code{lm}] model
#' (i.e., attempts to answer the question "how different was the start than the end?").
#'
#'
#' Rate is parameterized as a time constant, or the amount of time it takes for half of change to occur.
#' The value of rate has a lower bound of
#' the .0333 quantile of the time variable (i.e., 87.5\% of change happens in the first 10\% of time) and an upper bound of the
#' .333 quantile of the time variable (i.e., 87.5\% of change takes 100\% of the time to happen). These bounds provide
#' some robustness in estimates of asympototic effects (i.e., "controlling for time") as well as initial effects
#' (i.e., "time-related starting offset"). Uses this transformed time variable in a \code{\link{tef_rlm_boot}} model to estimate
#' bootstrapped parameter coefficients and out-of-sample prediction.
#'
#' @note
#' In \code{\link{TEfit}} and \code{\link{TEfitAll}} rate [50 percent time constant] is log2-transformed.
#' Here it is not.
#'
#' @seealso
#' \code{\link{TElmem}} for mixed-effects extension of \code{TElm};
#' \code{\link{TEglm}} for generalized extension of \code{TElm}
#'
#' @param formIn model formula, as in \code{lm()}
#' @param datIn model data, as in \code{lm()}
#' @param timeVar String. Indicates which model predictor is time (i.e., should be transformed)
#' @param robust  Logical. Should \code{\link[MASS]{rlm}} be used?
#' @param fixRate If numeric, use this as a rate parameter [50 percent time constant] rather than estimating it (e.g., to improve reproducibility)
#' @param nBoot Number of bootstrapped models to fit after rate [time constant] has been estimated
#'
#' @examples
#' dat <- data.frame(trialNum = 1:200, resp = log(11:210)+rnorm(200))
#' form <- resp ~ trialNum
#'
#' m_lm <- tef_lm(form,dat,'trialNum')
#' summary(m_lm)
#' m_lm$bootSummary
#' m_lm$rate
#'
#' m_rlm <- tef_lm(form,dat,'trialNum',robust=T)
#' summary(m_rlm)
#' m_rlm$bootSummary
#' m_rlm$rate
#'
#' plot(datIn[,c('trialNum','resp')])
#' lines(datIn$trialNum,fitted(m_lm),col='blue')
#' lines(datIn$trialNum,fitted(m_rlm),col='red')
#'
#' @export
TElm <- function(formIn,datIn,timeVar,robust=F,fixRate=NA,nBoot = 200){

  if(!is.numeric(fixRate)){suppressWarnings({
  fitRateLM <- function(rate,fitFormula,fitData,fitTimeVar,robust=robust){
    fitData[,fitTimeVar] <- 2^((1-fitData[,fitTimeVar])/rate)
    if(robust){ modErr <- sum(MASS::rlm(fitFormula,fitData)$residuals^2,na.rm=T) # minimize error (SSE)
    }else{modErr <- 1-summary(lm(fitFormula,fitData))$r.squared} # minimize error (1-rSquared)

    return(modErr)
  }

  bootRate <- replicate(100,{
    curDat <- datIn[sample(nrow(datIn),replace = T),]

    curFit <- NA ;  while(!is.numeric(curFit)){ # this increases robustness to pathological sampling
     curFit <- optimize(fitRateLM,
                  interval=quantile(curDat[,timeVar],c(1/30,1/3),na.rm=T), # 7/8 of learning has to happen in more than 10% of trials and less than 100% of trials
                  fitFormula=formIn,
                  fitData=curDat,
                  fitTimeVar=timeVar,
                  robust=robust
      )$minimum
    }
    curFit
  }
  )
  fixRate <- median(bootRate)
  })}

  datIn[,timeVar] <- 2^((1-datIn[,timeVar])/fixRate)


  ### ### still needs to be implemented: isn't playing will with interactions? covariates?
  if(robust){modOut <- tef_rlm_boot(formIn,datIn,nBoot = nBoot)
  }else{modOut <- tef_rlm_boot(formIn,datIn,nBoot = nBoot,useLM=T)}

  # if(robust){modOut <- MASS::rlm(formIn,datIn)
  # }else{modOut <- lm(formIn,datIn)}

  modOut$rate <- fixRate
  try({modOut$bootRate <- bootRate},silent = T)

  modOut$transformed_time <- datIn[,timeVar]

  modOut$call <- formula(deparse(formIn))

  return(modOut)
}


