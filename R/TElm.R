
#' [Robust] linear model with nonlinear time predictor
#'
#' Fit a linear model or robust linear model with time as a covariate,
#' while estimating the shape of the nonlinear interpolation between starting and ending time.
#' First resamples data with replacement 200 times, and each time estimates the best-shaped curve to interpolate
#' between initial time-related offset and asymptotic time (i.e., rate at which effect of time saturates at zero).
#' Then uses the mean estimated rate to transform the \code{timeVar} predictor into an exponentially decaying variable
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
#' Mean estimated rate is calculated after trimming the upper 25\% and lower 25\% of bootstrapped rate estimates, for robustness to
#' extremes in resampling.
#'
#' @note
#' The \code{TElm} approach to including a nonlinear time function in regression is quite different than the
#' \code{\link{TEfit}} approach. \code{TElm} utilizes a point estimate for the rate parameter in order to
#' coerce the model into a generalized linear format; \code{\link{TEfit}} simultaneously finds the best
#' combination of rate, start, and asympote parameters. In effect, \code{TElm} treats \emph{magnitude}
#' of change as being of theoretical interest (and rate as a nuisance parameter to be controlled for),
#' while \code{\link{TEfit}} treats the starting value, rate, and
#' the asymptotic value as each being of theoretical interest.
#'
#' @seealso
#' \code{\link{TElmem}} for mixed-effects extension of \code{TElm};
#' \code{\link{TEglm}} for generalized extension of \code{TElm}
#'
#' @param formIn model formula, as in \code{lm()}
#' @param dat model data, as in \code{lm()}
#' @param timeVar String. Indicates which model predictor is time (i.e., should be transformed). Must be numeric and positive.
#' @param robust  Logical. Should \code{\link[MASS]{rlm}} be used?
#' @param fixRate If numeric, use this as a rate parameter [binary-log of 50 percent time constant] rather than estimating it (e.g., to improve reproducibility)
#' @param nBoot Number of bootstrapped models to fit after rate [time constant] has been estimated (passed to \code{\link{tef_rlm_boot}})
#'
#' @examples
#' dat <- data.frame(trialNum = 1:200, resp = log(11:210)+rnorm(200))
#'
#' # using a Linear Model
#' m_lm <- TElm(resp ~ trialNum,dat, 'trialNum')
#' summary(m_lm)
#' m_lm$bootSummary
#' m_lm$rate
#'
#' # using a Robust Linear Model
#' m_rlm <- TElm(resp ~ trialNum,dat,'trialNum',robust=TRUE)
#' summary(m_rlm)
#' m_rlm$bootSummary
#' m_rlm$rate
#'
#' # comparing fits
#' plot(dat[,c('trialNum','resp')])
#' lines(dat$trialNum,fitted(m_lm),col='blue')
#' lines(dat$trialNum,fitted(m_rlm),col='red')
#'
#' # Examining the bootstrapped rates and other parameters together
#' summary(m_rlm$bootRate)
#' cor(m_rlm$bootRate)
#'
#' @export
TElm <- function(formIn,dat,timeVar,robust=F,fixRate=NA,nBoot = 250){

  minTime <- min(dat[,timeVar],na.rm = T)
  if(minTime < 0){cat('The earliest time is negative.')}

  if(!is.numeric(fixRate)){suppressWarnings({
    fitRateLM <- function(rate,fitFormula,fitData,fitTimeVar,robust=robust){
      fitData[,fitTimeVar] <- 2^((minTime-fitData[,fitTimeVar])/(2^(rate)))
      if(robust){ mod <- MASS::rlm(fitFormula,fitData)
      }else{mod <- lm(fitFormula,fitData)}
      return(-logLik(mod))
    }

    bootRate <- replicate(200,{ # resample data with replacement and find the best rate for that resampled data
      curRate <- NA ;  while(!is.numeric(curRate)){ # this increases robustness to pathological sampling
        curDat <- dat[sample(nrow(dat),replace = T),]
        curRate <- optimize(fitRateLM,
                            interval=quantile(log2(curDat[,timeVar]),c(1/30,1/3),na.rm=T), # 7/8 of learning has to happen in more than 10% of trials and less than 100% of trials
                            fitFormula=formIn,
                            fitData=curDat,
                            fitTimeVar=timeVar,
                            robust=robust
        )$minimum
      }
      curDat[,timeVar] <- 2^((minTime-curDat[,timeVar])/(2^(curRate)))
      if(robust){ mod <- MASS::rlm(formIn,curDat)
      }else{mod <- lm(formIn,curDat)}
      c(rate=curRate,coef(mod))
    }
    )
    bootRate <- as.data.frame(t(bootRate))
    fixRate <- mean(bootRate$rate,trim=.25)
  })}

  dat[,timeVar] <- 2^((minTime-dat[,timeVar])/(2^fixRate))

  modOut <- TEfits::tef_rlm_boot(formIn,dat,nBoot = nBoot,useLM=!robust)

  modOut$rate <- fixRate
  try({modOut$bootRate <- bootRate},silent = T)

  modOut$transformed_time <- dat[,timeVar]

  modOut$call <- formula(deparse(formIn))

  return(modOut)
}


