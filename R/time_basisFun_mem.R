
#' Fit a mixed-effects model with a set of by-time basis functions
#'
#' Given a typical [generalized] mixed-effects model, augment this model
#' using a random-effects structure including basis functions constructed 
#' from a vector of times (e..g, trial numbers).
#' This allows for simultaneously controlling for time-related variations, and for 
#' recovery of those variations from the fitted model.
#' 
#' Various fitting functions (\code{backends}) can be used, such as 
#' \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, and \code{\link[brms]{brm}}. 
#' These require that their respective packages and dependencies be installed.
#' 
#' In the case that out-of-sample likelihoods are desired, see 
#' `attr(modelObject,'delta_logLik_outOfSample_description')` for a description and
#' `attr(mod,'delta_logLik_outOfSample')` for the vector of out-of-sample delta-log-likelihoods. 
#' Out-of-sample log-likelihoods are calculated as an difference from the pointwise in-sample 
#' likelihood from a static fit (i.e., a model fit with only \code{formula_mem} and no bases).
#' This provides a uniform baseline pointwise in-sample likelihood against which various 
#' models (e.g., with different basis densities) can be compared.
#' 
#'
#' @param formula_mem Mixed-effects model formula, as in \code{\link[lme4]{glmer}} or \code{\link[brms]{brm}}
#' @param data_mem Data frame to fit model
#' @param ... Additional arguments to pass to the fitting function (e.g., \code{brm})
#' @param groupingVarName Character. Variable name, within \code{data_mem}, by which basis functions should be grouped (i.e., defined for each one).
#' @param timeVarName Character. Variable name, within \code{data_mem}, over which basis functions should be defined (e.g., trial number).
#' @param basisDens Numeric scalar. Distance between basis function peaks, on the same scale as \code{timeVarName}
#' @param basis_calc_fun Name of the method to calculate bases. Currently only "gaussian" is supported
#' @param backend Character. Name of fitting function (i.e., \code{lmer}, \code{glmer}, or \code{brm})
#' @param n_oos Numeric scalar. Number of times to re-fit the model with random 98/2 cross-validation. This provides an estimate of out-of-sample predictiveness.
#'
#' @export
#'
#' @examples 
#'  d <- data.frame(
#'      subID = rep(c('A','B','C'),each=200)
#'    , x = rbinom(600,1,.5)
#'    , sinOffset = sin((1:600)/10) # one possible kind of change: oscillation without trend
#'    , trialNum = rep(1:200, 3)
#' ) ; d$y <- rnorm(600) + d$x + d$sinOffset
#' m1 <- time_basisFun_mem(
#'    y ~ x + (x|subID)
#'    ,d
#'    ,groupingVarName = 'subID'
#'    ,timeVarName = 'trialNum'
#' )
#' 
#' ## overall model summary:
#' summary(m1)
#' 
#' ## extract the fitted timecourse:
#' m1_fitted_timeCourse <- predict(m1,random.only=T
#'    ,re.form = as.formula(paste('~',gsub('x + (x | subID) + ','',as.character(formula(m1))[3],fixed=T)) ) )
#' plot(d$trialNum,m1_fitted_timeCourse)
#' ## it doesn't look very good, because the default basis function width is fairly wide to prevent overfitting
#' 
#' ## let's compare two models' out-of-sample likelihoods and choose the best
#' 
#' ## The default, conservative, size (see m1)
#' m2 <- time_basisFun_mem(
#'    y ~ x + (x|subID)
#'    ,d
#'    ,groupingVarName = 'subID'
#'    ,timeVarName = 'trialNum'
#'    ,basisDens = 66
#'    ,n_oos = 50
#' )
#' 
#' ## A less-dense set of bases, every 20 trials
#' m3 <- time_basisFun_mem(
#'    y ~ x + (x|subID)
#'    ,d
#'    ,groupingVarName = 'subID'
#'    ,timeVarName = 'trialNum'
#'    ,basisDens = 20
#'    ,n_oos = 50
#' )
#' ## This takes noticeably longer to run
#' 
#' ## approximate Cohen's D of the out-of-sample log-likelihoods for m3 over m2:
#' (mean(attr(m3,'delta_logLik_outOfSample')) - mean(attr(m2,'delta_logLik_outOfSample')) )/
#'    sd(c(attr(m3,'delta_logLik_outOfSample'),attr(m2,'delta_logLik_outOfSample')))
#' ## Clearly m3's out-of-sample predictiveness is better than m2's
#' ## What about its fitted timecourse?
#' 
#' m3_fitted_timeCourse <- predict(m3,random.only=T
#'    ,re.form = as.formula(paste('~',gsub('x + (x | subID) + ','',as.character(formula(m3))[3],fixed=T)) ) )
#' plot(d$trialNum,m3_fitted_timeCourse)
#' ## These look much more like the sin-wave oscillations that were originally generated!
#' 
time_basisFun_mem <- function(formula_mem
                              ,data_mem
                              ,...
                              ,groupingVarName
                              ,timeVarName
                              ,basisDens = 'wide'
                              , basis_calc_fun='gaussian'
                              ,backend = c('lmer','glmer','brm')
                              ,n_oos = 0
){
  
  if(F){ ## for testing
    
    # now, just need to test!
    
    ## > doesn't break under various circumstances
    ## > > NAs
    ## > > non-integer and non-equally-spaced time variables
    ## > > unequal sample sizes for different groups
    ##
    ## > sensitivity: can recover effects when they exist
    ## > selectivity: 
    ## > > chooses very large bases, or no bases, when no change exists.
    ## > > does not overfit! (some small biases are fine, but large ones would be seriously problematic)
    ##
    ## > perhaps recommend stacking for brm() models? it should work great, but how to demonstrate?
    ## >
    
    # and make sure n_per_oos is accessible, and also include an optional progress for the oos
    
    data_in <- iris
    data_in$trial <- rep(1:50,3)
    
    m1 <- time_basisFun_mem(
      formula_mem = Sepal.Length ~ Petal.Length
      ,data_mem = data_in
      ,groupingVarName = 'Species'
      ,timeVarName = 'trial'
      ,basisDens = 12
    )
    m2 <- time_basisFun_mem(
      formula_mem = Sepal.Length ~ Petal.Length
      ,data_mem = data_in
      ,groupingVarName = 'Species'
      ,timeVarName = 'trial'
      ,n_oos = 50
    )
    
    m_basis25 <- time_basisFun_mem(
      formula_mem = Sepal.Length ~ Petal.Length
      ,data_mem = data_in
      ,groupingVarName = 'Species'
      ,timeVarName = 'trial'
      ,basisDens = 25
      ,n_oos = 50
    )
    m_basis10 <- time_basisFun_mem(
      formula_mem = Sepal.Length ~ Petal.Length
      ,data_mem = data_in
      ,groupingVarName = 'Species'
      ,timeVarName = 'trial'
      ,basisDens = 10
      ,n_oos = 50
    )
    
    median(attr(m_basis10,"delta_logLik_outOfSample"))
    median(attr(m_basis25,"delta_logLik_outOfSample"))
    ACmisc::wilcZ(attr(m_basis10,"delta_logLik_outOfSample")
                  ,attr(m_basis25,"delta_logLik_outOfSample"))
    
  }
  
  if(basisDens == 'wide'){
    basisDens <- floor((max(data_mem[,timeVarName],na.rm = T) - min(data_mem[,timeVarName],na.rm=T))/3)
  }
  
  formIn <- time_basisFun_formula(formula_mem = formula_mem
                                  # ,...
                                  ,groupingVar = groupingVarName
                                  ,timeVar = data_mem[,timeVarName]
                                  ,basisDens = basisDens
  )
  
  data_mem <- cbind(data_mem, attr(formIn, 'dfOut')[,2:ncol(attr(formIn,'dfOut'))])
  
  mod <- switch(backend[1]
                ,lmer = lmer(formula = formIn, data = data_mem)
                ,glmer = glmer(formula = formIn, data = data_mem,...)
                ,brm = brm(formula = formIn, data = data_mem,...,data2 = data_mem)
  )
  
  if(n_oos > 0){
    if(!exists('n_per_oos')){ ## note that I'm not sure this can inherit from `...`
      nperGroup <- xtabs(as.formula(paste0('~',groupingVarName)),data_mem)
      n_per_oos <- ceiling(mean(nperGroup)/50)
    }
    # if(!exists('progressBar')){ ## note that I'm not sure this can inherit from `...` and it also conflicts with a `psych` function!
    progressBar <- F
    # }
    mFull <- switch(backend[1]
                    ,lmer = lmer(formula = formula_mem, data = data_mem)
                    ,glmer = glmer(formula = formula_mem, data = data_mem,...)
                    ,brm = brm(formula = formula_mem, data = data_mem,...)
    )
    attr(mod,'m_no_time_bases') <- mFull
    
    lloos <- c()
    for(curOOS in 1:n_oos){
      dTmp <- data_mem
      
      ## first, build vector with Ts if oos and Fs if ins
      {
        dTmp$inSample <- T
        for(curGroup in names(nperGroup)){
          dTmp[dTmp[,groupingVarName] == curGroup
               ,'inSample'][sample(nperGroup[curGroup],n_per_oos)] <- F
        }
      }
      ## second, fit model to ins
      suppressWarnings(suppressMessages({
        lloos[length(lloos)+1] <- switch(backend[1]
                                         ,lmer = {
                                           mA <- lmer(formula = formIn, data = dTmp[dTmp$inSample,]) # in-sample mod
                                           
                                           mAoos_resid <- dTmp[!dTmp$inSample,as.character(formula_mem)[2]] - # out-of-sample prediction residual
                                             predict(mA,dTmp[!dTmp$inSample,])
                                           mFull_resid <- dTmp[!dTmp$inSample,as.character(formula_mem)[2]] - # in-sample prediction residual
                                             predict(mFull,dTmp[!dTmp$inSample,])
                                           
                                           llAoos <- sum(dnorm(mAoos_resid,sd=sd(mFull_resid),log=TRUE)) # out-of-sample LL
                                           llAins <- sum(dnorm(mFull_resid,sd=sd(mFull_resid),log=TRUE)) # full-sample LL
                                           curDeltaLLoos <- llAoos - llAins # difference between full and oos
                                           rm(mAoos_resid,mFull_resid,llAoos,llAins,mA)
                                           curDeltaLLoos
                                         }
                                         ,glmer = stop('out-of-sample likelihood is not yet implemented for glmer models')
                                         ,brm = {
                                           mA <- brm(formula = formIn, data = dTmp[dTmp$inSample,],...,data2 = data_mem)
                                           
                                           llAoos <- colSums(log_lik(mA,newdata = dTmp[!dTmp$inSample,])) # out-of-sample LL
                                           llAins <- colSums(log_lik(mFull,newdata = dTmp[!dTmp$inSample,])) # full-sample LL
                                           curDeltaLLoos <- mean(llAoos - llAins) # difference between full and oos
                                           rm(mAoos,mAins,llAoos,llAins,mA)
                                           curDeltaLLoos
                                         }
        )
      }))
      
      if(progressBar){cat('=')}
    }
    ## fifth, combine into sensible output
    attr(mod,'delta_logLik_outOfSample') <- lloos
    attr(mod,'delta_logLik_outOfSample_description') <- paste0(
      'For each of the `',groupingVarName,'` grouping variable, ',n_per_oos,' randomly-chosen case(s) are held out for ',
      'each of ',n_oos,' fits. Each held-out case has its predicted likelihood ',
      'compared to the likelihood for that case in the full model (the in-sample likelihood from a model with '
      ,'no basis functions over time). For each of the ',
      n_oos,' fitting runs, the mean is calculated for the difference between the casewise ',
      "out-of-sample likelihoods and the full time-insensitive model's in-sample likelihoods. ",
      'This method is used to be able to compare out-of-sample likelihoods across models with different ',
      'basis function densities (i.e., by having a common baseline), while not needing to re-fit the baseline ',
      'model many times.'
    )
    if(backend[1] == 'lmer'){
      attr(mod,'delta_logLik_outOfSample_description') <- paste(attr(mod,'delta_logLik_outOfSample_description')
                                                                ,'Both full-model and out-of-sample log-likelihoods are calculated using a heuristic method:'
                                                                ,'`dnorm(residuals,sd=sd(residuals),log=TRUE)`')
    }
  }
  
  attr(mod,'orig_data') <- data_mem
  
  return(mod)
}
