
#' Generalized linear model with nonlinear time predictor
#'
#' Fit a generalized linear model with time as a covariate,
#' while estimating the shape of the nonlinear interpolation between starting and ending time.
#' First resamples data with replacement 200 times, and each time estimates the best-shaped curve to interpolate
#' between initial time-related offset and asymptotic time (i.e., rate at which effect of time saturates at zero).
#' Then uses the median estimated rate to transform the \code{timeVar} predictor into an exponentially decaying variable
#' interpolating between initial time (time offset magnitude of 1) and arbitrarily large time values (time
#' offset magnitude 0). Last uses this transformed time variable in a \code{rlm} or \code{lm} model
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
#' @param formIn model formula, as in glm()
#' @param datIn model data, as in glm()
#' @param timeVar String. Indicates which model predictor is time (i.e., should be transformed)
#' @param family  passed to glm()
#' @param fixRate If numeric, use this as a rate parameter [50 percent time constant] rather than estimating it (e.g., to improve reproducibility)
#'
#' @note
#' In \code{\link{TEfit}} and \code{\link{TEfitAll}} rate [50 percent time constant] is binary-log-transformed.
#' Here it is not.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(trialNum = 1:200, resp = rbinom(200,1,log(11:210)/log(300)))
#' m_glm <- TEglm(resp ~ trialNum,dat,'trialNum',family=binomial)
#' m_glm$rate # estimated half-of-change time constant
#'
TEglm <- function(formIn,datIn,timeVar,family=gaussian,fixRate=NA){

  if(!is.numeric(fixRate)){suppressWarnings({
  fitRateLM <- function(rate,fitFormula,fitData,fitTimeVar,family=family){
    fitData[,fitTimeVar] <- 2^((1-fitData[,fitTimeVar])/rate)
    modErr <- -logLik(glm(fitFormula,fitData,family=family)) # minimize error ( -logLikelihood)

    return(modErr)
  }

  bootRate <- replicate(200,{# resample data with replacement and find the best rate for that resampled data
      curFit <- NA ;  while(!is.numeric(curFit)){ # this increases robustness to pathological sampling
    curDat <- datIn[sample(nrow(datIn),replace = T),]
     curFit <- optimize(fitRateLM,
                  interval=quantile(curDat[,timeVar],c(1/30,1/3),na.rm=T), # 7/8 of learning has to happen in more than 10% of trials and less than 100% of trials
                  fitFormula=formIn,
                  fitData=curDat,
                  fitTimeVar=timeVar,
                  family=family

      )$minimum
    }
    curFit
  }
  )
  fixRate <- median(bootRate)
  })}

  datIn[,timeVar] <- 2^((1-datIn[,timeVar])/fixRate)


  ### ### still needs to be implemented: isn't playing will with interactions? covariates?
  # if(robust){modOut <- tef_rlm_boot(formIn,datIn,nBoot = nBoot)
  # }else{modOut <- tef_rlm_boot(formIn,datIn,nBoot = nBoot,useLM=T)}

  modOut <- glm(formIn,datIn,family=family)

  modOut$rate <- fixRate
  try({modOut$bootRate <- bootRate},silent = T)

  modOut$transformed_time <- datIn[,timeVar]

  modOut$call <- formula(deparse(formIn))

  return(modOut)
}


