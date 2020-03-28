

#' Bootstrapped MASS::rlm
#'
#' Run a \code{MASS::rlm} model \code{nBoot} times, then include the bootstrapped parameter estimates
#' (and summary of quantiles thereof) in the rlm output. Also includes out-of-sample
#' delta-R-squared. If parameter distributions are *anywhere near* decision thresholds,
#' using \code{nBoot}>2000 (or even much higher) is recommended.
#'
#' Wraps rlm() and then bootstraps [fits models to datasets sampled with replacement]
#' that model \code{nBoot} times. Then fits models to
#' nBoot random 80 percent of data and tests the delta R-squared of each parameter
#' when predicting the out-of-sample 20 percent. An augmented \code{rlm} object is returned that includes
#' several new items: \code{$bootSummary}, \code{$boots} (all bootstrapped parameters),
#' \code{$bootQs} (quantiles
#' of bootstrapped parameters), \code{$dRsq} (all out-of-sample proportional reduction of error),
#' \code{$dRsqQs} (quantiles of out-of-sample proportional reduction of error),
#' and \code{$results} (strings, formatted
#' for RMarkdown, including whole-sample slope, bootstrapped CI, and median dRSq).
#'
#' For an explanation specific objects see \code{comment(model$boots)},
#' \code{comment(model$dRsq)}, or \code{comment(model$bootSummary)}.
#'
#' @param formIn Model formula, as with lm() or rlm()
#' @param datIn Data, as with lm() or rlm()
#' @param nBoot Number of resamples [with replacement]
#' @param useLM Override the standard MASS::rlm() implementation to use basic lm() instead
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x=rnorm(50))
#' dat$y <- dat$x + rnorm(50)
#' dat$z <- dat$y - rnorm(50)
#' dat$z[2] <- NA
#' m <- tef_rlm_boot(y~x*z,dat)
#' m$bootSummary # to get a summary of the fit[s]
#' comment(m$bootSummary) # to get an explanation of the summary data frame
#'
tef_rlm_boot <- function(formIn,datIn,nBoot=500,useLM=F){

  if(!is.numeric(nBoot)){cat('Your number of boots must be numeric.')}
  if(nBoot<2){(cat('Your number of boots must be positive'))}
  nBoot <- round(nBoot)

  if(useLM){fitReg <- lm
  }else{
    require(MASS)
    fitReg <- rlm}

  m <- suppressWarnings({fitReg(formIn,datIn,maxit=1E4)})


  ## bootstrapped fit:
  {
    boots <- replicate(nBoot,
                       suppressWarnings({
                         curCoef <- NA
                         while(!is.numeric(curCoef)){
                           try({
                             curRLM <- suppressWarnings({fitReg(formIn,datIn[sample(nrow(datIn),replace=T),],maxit=50)})
                             curCoef <- curRLM$coefficients
                             # curPred <- fitted(curRLM)
                           },silent=T)
                         }
                         return(#list(
                           curCoef=curCoef
                           #,curPred=curPred)
                         )
                       })
    )
    m$boots <- data.frame( t(boots) )
    colnames(m$boots) <- names(m$coefficients)
    comment(m$boots) <- paste('Parameter estimates from',nBoot,
                              'MASS::rlm fits to data resampled with replacement.')
    m$bootQs <-apply(na.omit(data.frame(t(m$boots))),1,quantile,probs=c(.025,.25,.5,.75,.975))
  }
  # # # #
  ## out-of-sample delta R squared:
  {
    oos_dRsq <- replicate(nBoot,{
      outVar <- as.character(formIn[2])

      shuffleInds <- sample(nrow(datIn))
      trainInds <- shuffleInds[1:(round(nrow(datIn))*.8)]
      testInds <- shuffleInds[((round(nrow(datIn))*.8)+1):nrow(datIn)]

      sse_raw <- sum((datIn[testInds,outVar]-mean(datIn[testInds,outVar],na.rm=T))^2,na.rm=T)

      fullMod_preds <- predict(suppressWarnings({fitReg(formIn,datIn[trainInds,],maxit=50)}),
                               datIn[testInds,])
      sse_fullMod <- sum((datIn[testInds,outVar]-fullMod_preds)^2,na.rm=T)

      dRsq <- c(fullMod = (sse_raw-sse_fullMod)/sse_raw)

      parNames <- names(m$coefficients)[2:length(names(m$coefficients))]

      if(length(parNames)>1){tilde<- '~'}else{tilde <- '~ 1'}
      for (dropVar in 1:length(parNames)){

        curForm <- formula(paste(outVar,tilde,paste(parNames[(1:length(parNames))!=dropVar],collapse='+')))

        curMod_preds <- predict(suppressWarnings({fitReg(curForm,datIn[trainInds,],maxit=50)}),
                                datIn[testInds,])
        sse_curMod <- sum((datIn[testInds,outVar]-curMod_preds)^2,na.rm=T)

        dRsq[parNames[dropVar]] <- (sse_curMod - sse_fullMod)/sse_curMod

      }
      dRsq
    })

    m$dRsq <- data.frame(t(oos_dRsq))
    comment(m$dRsq) <- paste('Parameter estimates from MASS::rlm fits to',nBoot,
                             'random 80% subsamples of data (without replacement).',
                             'Fits using all predictors were compared to fits dropping',
                             'each predictor in turn. Out-of-sample delta R-squared was',
                             'calculated on each remaining 20% of data by comparing',
                             'full parameter set fits to drop-one parameter set fits.')
    m$dRsqQs <-apply(na.omit(data.frame(t(m$dRsq))),1,quantile,probs=c(.025,.25,.5,.75,.975))
  }
  ## ## ##
  # # # #
  ## summary:
  {
    outDF <- data.frame(summary(m)$coefficients)[,1:3]
    outDF$ci025 <- m$bootQs["2.5%", ]
    outDF$ci975 <- m$bootQs["97.5%", ]
    outDF$bootP <- NA

    for (curCoef in 1:ncol(m$boots)) {
      curECDF <- ecdf(m$boots[, curCoef])
      outDF[curCoef, "bootP"] <- min(curECDF(0),
                                     1 - curECDF(0)) * 2
    }
    outDF[, c(1, 2, 4, 5)] <- signif(outDF[, c(1, 2, 4, 5)],
                                     2)
    outDF[, 3] <- round(outDF[, 3], 2)
    outDF[, 6] <- round(outDF[, 6], 3)
    outDF$dRsq_oos <- round(m$dRsqQs['50%',],4)
    outDF$dRsq_oos[1] <- NA
    comment(outDF) <- paste('Summary of rlm_boot. The first 3 columns are from the standard rlm fit.',
                            'ci025 and ci975 are the 2.5% and 97.5% quantiles of parameter estimates to bootstrapped fits.',
                            'BootP indicates the quantile (calculated from the parameter distributions) associated with a',
                            '0 slope for a given parameter, multiplied by 2 to imitate a conventional 2-tailed p value.',
                            'dRsq_oos is the median out-of-sample prediction error reduction when including a',
                            'given parameter in the full model. This is calculated as a proportional',
                            'reduction of squared error in out-of-sample prediction',
                            'relative to a model including all parameters except the specific parameter in question. ')

    m$bootSummary <-outDF

    formattedResults <- c()
    if(useLM){bname='Estimate'}else{bname='Value'}
    for (curCoef in 1:ncol(m$boots)) {
      coefName <- colnames(m$boots)[curCoef]
      formattedResults <- c(formattedResults,
                            paste0('*b* = ',outDF[coefName,bname],', CI = [',
                                   paste(outDF[coefName,c('ci025','ci975')],collapse=','),'], dR^2^~oos~ = ',
                                   outDF[coefName,c('dRsq_oos')])
      )
    }
    names(formattedResults) <- colnames(m$boots)
    m$results <- formattedResults
  }


  return(m)


}
