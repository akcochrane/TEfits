
#' Generalized linear model with nonlinear time predictor
#'
#' Fit a generalized linear model with time as a covariate,
#' while estimating the shape of the nonlinear interpolation between starting and ending time.
#' First resamples data with replacement 200 times, and each time estimates the best-shaped curve to interpolate
#' between initial time-related offset and asymptotic time (i.e., rate at which effect of time saturates at zero).
#' Then uses the mean estimated rate to transform the \code{timeVar} predictor into an exponentially decaying variable
#' interpolating between initial time (time offset magnitude of 1) and arbitrarily large time values (time
#' offset magnitude 0). Last uses this transformed time variable in a \code{glm} model
#' (i.e., attempts to answer the question "how different was the start than the end?").
#'
#' Rate is parameterized as a time constant, or the amount of time it takes for half of change to occur.
#' The value of rate has a lower bound of
#' the .0333 quantile of the time variable (i.e., 87.5\% of change happens in the first 10\% of time) and an upper bound of the
#' .333 quantile of the time variable (i.e., 87.5\% of change takes 100\% of the time to happen). These bounds provide
#' some robustness in estimates of asympototic effects (i.e., "controlling for time") as well as initial effects
#' (i.e., "time-related starting offset"). Uses this transformed time variable in a \code{\link{tef_rlm_boot}} model to estimate
#' bootstrapped parameter coefficients and out-of-sample prediction.
#'
#' Mean estimated rate is calculated after trimming the upper 25\% and lower 25\% of bootstrapped rate estimates, for robustness to
#' extremes in resampling.
#'
#' @note
#' Although the time variable is transformed to exponentially decay toward zero, this does not necessarily mean
#' that the model prediction involves an exponential change with time. The nonlinear change in time relates
#' to the time-associated model coefficients.
#'
#' The \code{TEglm} approach to including a nonlinear time function in regression is quite different than the
#' \code{\link{TEfit}} approach. \code{TEglm} utilizes a point estimate for the rate parameter in order to
#' coerce the model into a generalized linear format; \code{\link{TEfit}} simultaneously finds the best
#' combination of rate, start, and asympote parameters. In effect, \code{TEglm} treats \emph{magnitude}
#' of change as being of theoretical interest, while \code{\link{TEfit}} treats the starting value, rate, and
#' the asymptotic value as each being of theoretical interest.
#'
#' @seealso
#' \code{\link{TEglmem}} for mixed-effects extension of \code{TEglm};
#' \code{\link{TElm}} for a linear model version of \code{TEglm}
#'
#' @param formIn model formula, as in glm()
#' @param dat model data, as in glm()
#' @param timeVar String. Indicates which model predictor is time (i.e., should be transformed)
#' @param family  passed to glm()
#' @param fixRate If numeric, use this as a rate parameter [binary-log of 50 percent time constant] rather than estimating it (e.g., to improve reproducibility)
#'
#' @export
#'
#' @examples
#' dat <- data.frame(trialNum = 1:200, resp = rbinom(200,1,log(11:210)/log(300)))
#' m_glm <- TEglm(resp ~ trialNum,dat,'trialNum',family=binomial)
#' summary(m_glm)
#' m_glm$rate # estimated half-of-change time constant
#' summary(m_glm$bootRate) # bootstrapped parameter distributions
#' cor(m_glm$bootRate) # bootstrapped parameter correlations
#'
TEglm <- function(formIn,dat,timeVar,family=gaussian,fixRate=NA){

  minTime <- min(dat[,timeVar],na.rm = T)
  if(minTime < 0){cat('The earliest time is negative.')}

  if(!is.numeric(fixRate)){suppressWarnings({

  fitRateLM <- function(rate,fitFormula,fitData,fitTimeVar,family=family){
    fitData[,fitTimeVar] <- 2^((minTime-fitData[,fitTimeVar])/(2^rate))
    modErr <- -logLik(glm(fitFormula,fitData,family=family)) # minimize error ( -logLikelihood)

    return(modErr)
  }

  bootRate <- replicate(200,{# resample data with replacement and find the best rate for that resampled data
      curRate <- NA ;  while(!is.numeric(curRate)){ # this increases robustness to pathological sampling
    curDat <- dat[sample(nrow(dat),replace = T),]
     curRate <- optimize(fitRateLM,
                  interval=quantile(log2(curDat[,timeVar]),c(1/30,1/3),na.rm=T), # 7/8 of learning has to happen in more than 10% of trials and less than 100% of trials
                  fitFormula=formIn,
                  fitData=curDat,
                  fitTimeVar=timeVar,
                  family=family

      )$minimum
      }
      curDat[,timeVar] <- 2^((minTime-curDat[,timeVar])/(2^curRate))
      mod <- glm(formIn,curDat,family=family)
      c(rate=curRate,coef(mod))
  }
  )
  bootRate <- as.data.frame(t(bootRate))
  fixRate <- mean(bootRate$rate,trim=.25)
  })}

  dat[,timeVar] <- 2^((minTime-dat[,timeVar])/(2^fixRate))


  ### ### still needs to be implemented: isn't playing will with interactions? covariates?
  # if(robust){modOut <- tef_rlm_boot(formIn,dat,nBoot = nBoot)
  # }else{modOut <- tef_rlm_boot(formIn,dat,nBoot = nBoot,useLM=T)}

  modOut <- glm(formIn,dat,family=family)

  modOut$rate <- fixRate
  try({modOut$bootRate <- bootRate},silent = T)

  modOut$transformed_time <- dat[,timeVar]

  modOut$call <- formula(deparse(formIn))

  return(modOut)
}


