
#' Fit a time-evolving model (nonlinear regression by minimizing error)
#'
#' This is the primary function for the \code{TEfits} package (but see \code{\link{TEbrm}} for
#' a more powerful approach). Fits a
#' time-evolving regression model. Many options are available for
#' various error functions, functional forms of change,
#' nested timescales, bootstrapping/subsampling/cross-validation, and so on.
#' Various handy S3 methods are available, such as
#' \code{plot}, \code{summary},
#' \code{coef}, and \code{simulate}.
#'
#' TEfit defines a nonlinear regression model and re-fits that model
#' using \code{optim} numerous times, with parameter values randomly initialized prior to optimization, until
#' the highest-likelihood fitting runs also have parameters very similar to
#' one another (i.e., SD less than the convergence criterion). Runs are
#' implemented in batches of 10. Convergence is a heuristic and should ideally be
#' corroborated with other measures (e.g., bootstrapping).
#'
#' Bootstrapping or subsampling is specified as
#' \code{bootPars=tef_bootList(resamples = 0, bootPercent = 1, bootTries = 20)}.
#' \code{resamples} refers to the number of times the model is re-fit on resampled data,
#' \code{bootPercent} is the proportion (between 0 and 1) of the data resampled, and
#' \code{bootTries} is the number of optimization runs attempted on each subsample.
#' \code{bootPercent} of 1, the default, implements resampling with replacement (bootstrapping).
#' \code{bootPercent} less than 1 implements resampling without replacement, fitting
#' the model to that subsample, and evaluation of the fit values on the
#' left-out subsample (i.e., cross-validation). \code{bootTries} defaults to a very small number (20).
#'
#'Currently supported \strong{error functions} are:
#'\itemize{
#'\item{\code{ols}, i.e. \code{sum((y-yHat)^2)} -- sum of squared error}
#'\item{\code{rmse}, i.e. \code{sqrt(mean((y-yHat)^2))} -- root mean squared error}
#'\item{\code{logcosh}, i.e. \code{sum(log(cosh(y-yHat)))} -- log-hyperbolic-cosine}
#'\item{\code{bernoulli}, i.e. \code{-sum(y*log(yHat) + (1-y)*log(1-yHat))} -- Bernoulli [binary binomial]}
#'\item{\code{exGauss_mu}, i.e. \code{-sum(log(retimes::dexgauss(y,mu=yHat,sigma=sigma_param,tau=tau_param)))} --
#'ex-Gaussian distribution with time-evolving change in the Gaussian mean parameter}
#'\item{\code{exGauss_tau}, i.e. \code{-sum(log(retimes::dexgauss(y,mu=mu_param,sigma=sigma_param,tau=yHat)))} --
#'ex-Gaussian distribution with time-evolving change in the tau parameter}
#'}
#'
#'Currently supported \strong{link functions} are:
#' \itemize{
#' \item{\code{identity} -- implemented by \code{linkFun=list(link='identity')} --
#' Default. The predicted values of the time-evolving function are the predicted
#' values of the model.}
#'
#' \item{\code{logit} -- implemented by
#' \code{linkFun=list(link='logit',logistX='variableName',
#' threshChange=T,biasChange=F,fitThresh=.75,lapseRate=.005)}
#' -- The predicted values of the time-evolving function are threshold values
#' of \code{logistX} (by default) and/or
#' the bias value (defaults to constant). \code{link} and \code{logistX} are required.
#' Other parameters have default values
#' (i.e., modelled value of the outcome variable [\code{fitThresh}],
#' offset of the predicted values to prevent a pathological error calculation [\code{lapseRate}])
#' }
#' \item{\code{weibull} -- implemented by
#' \code{linkFun=list(link='weibull',weibullX='variableName',
#' fitThresh=.75,yIntercept=.5,rhAsymptote=1,lapseRate=.005)}
#' -- The predicted values of the time-evolving function are threshold values
#' of \code{weibullX}.
#' \code{link} and \code{weibullX} are required. Other parameters
#' have default values
#' (i.e., modelled value of the outcome variable [\code{fitThresh}],
#' value of the outcome variable at weibullX==0 [\code{yIntercept}],
#' value of the outcome variable at weibullX==Inf [\code{rhAsymptote}],
#' offset of the predicted values to prevent a pathological error calculation [\code{lapseRate}])
#' .}
#' \item{\code{d_prime} -- implemented by
#' \code{linkFun=list(link='d_prime',presence='variableName',
#' max_d_prime=5,smooth_hwhm=3)}
#' -- \code{link} and \code{presence} are required, \code{max_d_prime}
#' and \code{smooth_hwhm} have default values. The pFA and pH are
#' first calculated using a windowed average of stimulus-present or
#' stimulus-absent trials (penalized to bound the max d-prime), then calculating the
#' by-timepoint d-prime, then fitting that d-prime as the response variable using an identity link. See
#' \code{\link{tef_acc2dprime}} and \code{\link{tef_runningMean}} for details about the intermediate steps.}
#' }
#'
#' Currently supported \strong{change functions} are:
#' \itemize{
#' \item{\code{expo} -- 3-parameter exponential (start, [inverse] rate, and asymptote) -- rate is log of time to some proportion remaining, default is log2 of time to 50 percent remaining}
#' \item{\code{expo_block} -- 3-parameter exponential (start, [inverse] rate, and asymptote)
#' plus 2-paramter multiplicative changes on timescales that are a subset of the whole}
#' \item{\code{expo_double} -- 4-parameter exponential (start, two equally weighted [inverse] rates, and asymptote)}
#' \item{\code{power} -- 3-parameter power (start, [inverse] rate, and asymptote) -- rate is log of time to some proportion remaining, defaulting to log2 of time to 25 percent remaining}
#' \item{\code{power4} -- 4-parameter power (start, [inverse] rate, asymptote, and "previous learning time")}
#' \item{\code{weibull} -- 4-parameter weibull (start, [inverse] rate, asymptote, and shape) -- rate is same as \code{expo}}
#' }
#'
#' @note
#' By default, the mean of the time-evolving model's fit values should be very similar to the mean of the null fit values.
#' This is enforced by penalizing the time-evolving model's error multiplicatively by 1 + the square of the difference
#' between the average of the model prediction and the average of the null [non-time-evolving] prediction. This is intended to
#' constrain model predictions to a "sane" range. This constraint can be removed with \code{control=tef_control(penalizeMean=F)}.
#'
#' Currently \strong{known bugs} are:
#' \itemize{
#' \item{The logistic linkfun must include threshChange=T. Returns error if only the bias term is allowed to change.}
#' }
#'
#' @param varIn   Data frame or vector. First column [or vector] must be the time-dependent response variable (left hand side of regression). If available, second column must be the time variable. All other columns are covariates, possibly involved in a link function.
#' @param linkFun A list defining a link function (i.e., 'identity', 'd_prime', 'weibull', or 'logistic')
#' @param errFun  A string defining an error function (e.g., 'ols', 'logcosh', 'bernoulli').
#' @param changeFun A string defining the functional form of change (e.g., 'expo', 'power', 'weibull')
#' @param bootPars A list defining the details for bootstrapped fits. Defaults to no bootstrapping. Necessary for estimates of uncertainty around fits and for covariance between parameters.
#' @param blockTimeVar A string identifying which covariate is the time points of sub-scales (e.g., "blocks" of times within the overall timescale of data collection)
#' @param covarTerms An optional list of logical vectors indicating whether parameters should vary by covariates. See examples.
#' @param control A list of model parameters. Use of tef_control() is highly recommended.
#'
#' @return
#' A \code{TEfit} S3 object including:
#' \describe{
#' \item{\code{model}}{The best fit of parameters to the dataset, along with associated items such as tests of goodness-of-fit and nonstationarity}
#' \item{\code{nullFit}}{The best fit of a model that does not change over time, but otherwise uses the same parameterization as \code{model}}
#' \item{\code{data}}{Data frame of the input variables fit by the model, with the additiona of fit values}
#' \item{\code{modList}}{List of model details}
#' \item{\code{bootList}}{\emph{(if relevant)} List of bootstrapped \code{TEfit} models.}
#' }
#'
#' @seealso
#' For interpreting model outputs: \code{\link{plot.TEfit}}; \code{\link{summary.TEfit}};
#' \code{\link{coef.TEfit}}; \code{\link{simulate.TEfit}}
#'
#' \code{\link{TEfitAll}} for fitting a set of \code{TEfit} models.
#'
#' \code{\link{TEbrm}} for fitting a Bayesian regression model, with many options
#' including fixed and random effects.
#'
#' For including a nonlinear time predictor in [generalized] linear regression frameworks:
#' \code{\link{TElm}}; \code{\link{TEglm}}; \code{\link{TElmem}}; \code{\link{TEglmem}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## example data:
#' dat <- data.frame(timeVar = 1:50, respVar = c(seq(.3,.9,length=25),seq(.9,.91,length=25))+rep(c(0,.01),25),covar1=rep(c(1,2),25),covar2 = rep(c(-3,-1,0,2,5),10))
#'
#' ## Default fitting of 'ols' error function and 3-parameter exponential change function:
#' m <- TEfit(dat[,c('respVar','timeVar')])
#' summary(m)
#' # view a plot of the model:
#' plot(m)
#'
#' ## 'bernoulli' error function:
#' m <- TEfit(dat[,c('respVar','timeVar')],errFun='bernoulli')
#' summary(m)
#'
#' ## 3-parameter power change function:
#' m <- TEfit(dat[,c('respVar','timeVar')],changeFun='power')
#' summary(m)
#' # view a plot of the model:
#' plot(m)
#'
#' ## logistic threshold change:
#' m <- TEfit(dat[,c('respVar','timeVar','covar2')],errFun='bernoulli',linkFun=list(link='logit',logistX='covar2'))
#'
#' ## include 2 covariates (on all 3 parameters by default):
#' m <- TEfit(dat[,c('respVar','timeVar','covar1','covar2')])
#' summary(m) # (likely does not converge due to too many [nonsense] covariates)
#' plot(m)
#'
#' ## include 2 covariates:
#' ## asymptote and rate are affected by covar1, start and rate are affected by covar2
#' m <- TEfit(dat[,c('respVar','timeVar','covar1','covar2')],covarTerms=list(pStart=c(F,T),pRate=c(T,T),pAsym=c(T,F)))
#'
#' ## 50 bootstrapped fits:
#'  m <- TEfit(dat[,c('respVar','timeVar')],bootPars=tef_bootList(resamples=50))
#'  summary(m)
#' # view a plot of the model, with CI bands:
#' plot(m)
#' # view the predicted values of the model by plotting data simulated from the parameters:
#' . <- simulate(m,toPlot=T)
#'
#'  ## 50 random-subsample 80/20 cross-validation fits:
#'  m <- TEfit(dat[,c('respVar','timeVar')],bootPars=tef_bootList(resamples=50,bootPercent=.8))
#'  summary(m)
#'
#'  ## ## ## control parameters:
#'
#'  ## Increase convergence tolerance to 0.1:
#'  m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(convergeTol=.1))
#'
#'  ## Increase the maximum run number to 5000 (defaults to 200):
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(nTries=5000))
#'
#'  ## If the function will asymptote in the given time period, then one option is to calculate the TE function stepwise: first get a stable fit of last 20% of timepoints (if there are enough timepoints, average this with a stable fit to the last 10% of timepoints). Then fit the start and rate of approach to this asymptote:
#'  m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(stepwise_asym = T))
#'
#'  ## Put limits on the predicted values:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(y_lim=c(.1,.9)))
#'
#'  ## Put limits on the rate parameter:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(rate_lim=c(2,4)))
#'
#'  ## Remove the constraint that the time-evolving fit values should have the same mean as the null fit values:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(penalizeMean=F))
#'
#'  ## If rate parameter is hitting the boundary, try imposing a slight penalization for extreme rate values:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(penalizeRate=T))
#'
#'  ## Change the exponential change log base from 2 to e:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(expBase=exp(1)))
#'
#'  ## Change the rate parameter log base from 2 to e:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(rateBase=exp(1)))
#'
#'  ## Silence errors:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(quietErrs=T))
#'
#'  ## Fix a parameter [asymptote] to 0.8:
#'   m <- TEfit(dat[,c('respVar','timeVar')],control=tef_control(pFix=list(pAsym=.8)))
#' }
TEfit <- function(varIn,
                  linkFun = list(link='identity'),
                  errFun = 'ols',
                  changeFun = 'expo',
                  bootPars = tef_bootList(),
                  blockTimeVar = NULL,
                  covarTerms = list(),
                  control=tef_control()
){

  modList <- list()
  modList$times <- c()
  modList$times['start'] <- Sys.time()

  ## ## ## ## ## ## ## ## ##
  ## ## >arguments:
  {
    modList$linkFun <- linkFun
    modList$errFun <- errFun
    modList$changeFun <- changeFun
    modList$bootPars <- bootPars
    modList$blockTimeVar <- blockTimeVar
    modList$covarTerms <- covarTerms

    modList$varIn <- varIn
    # # add in trial num, if not included in varIn
    if(is.vector(modList$varIn)){
      modList$varIn <- data.frame(y=modList$varIn,timeVar=1:length(modList$varIn))
    }
    if(any(sapply(modList$varIn,class)=='factor')){warning('your data includes factors. Please do not include factor variables.')}
  }## ## ##
  ## ^^ ^^ ^^ ^^ ^^ ^^ ##

  ## ## ## ## ## ## ## ## ##
  ## ## >control arguments:
  {
    if(length(control) < length(tef_control())){stop('\nYou do not have enough control inputs. Please use `control=tef_control()`.\n')}
    modList$convergeTol <- control$convergeTol
    modList$nTries      <- control$nTries
    modList$y_lim       <- control$y_lim
    modList$rate_lim    <- control$rate_lim
    modList$shape_lim   <- control$shape_lim
    modList$expBase     <- control$expBase
    modList$rateBase    <- control$rateBase
    modList$pFix        <- control$pFix
    modList$stepwise_asym<- control$stepwise_asym
    modList$penalizeRate<- control$penalizeRate
    modList$penalizeMean<- control$penalizeMean
    modList$explicit    <- control$explicit
    modList$quietErrs   <- control$quietErrs
    modList$suppressWarnings <- control$suppressWarnings
  }
  ## ^^ ^^ ^^ ^^ ^^ ^^ ##

  ## ## ## ## ## ## ## ## ##
  ## ## >name your response variable and your time variable
  modList$respVar <- colnames(modList$varIn)[1]
  modList$timeVar <- colnames(modList$varIn)[2]

  if(max(xtabs(~modList$varIn[,1]),na.rm=T)/dim(na.omit(modList$varIn))[1]>.9){
    warning('\nYour response variable has few unique values. You may not be able to estimate a time-evolving function.\n')
  }
  ## ^^ ^^ ^^ ^^ ^^ ^^ ##

  if(modList$errFun=='bernoulli'){
    modList$y_lim[1] <- max(modList$y_lim[1],0)
    modList$y_lim[2] <- min(modList$y_lim[2],1)
  }

  modList$times['before_vars2forms'] <- Sys.time() - modList$times['start']
  modList <- tef_vars2forms(modList)

  ## ## ## ## ## ## ## ## ##
  ## ## > stepwise asymptote option
  #### #### ## NEEDS TO BE error checked.
  if(modList$stepwise_asym){
    if(nrow(modList$varIn)>20){ ## fit stable asymptote to the last 20% (at least 4 obs)
      asymModList <- modList
      asymModList$varIn <- asymModList$varIn[order(asymModList$varIn[,'timeVar'])[round(nrow(asymModList$varIn)*.8):nrow(asymModList$varIn)],]
      stableAsymFit <- tef_tryFits(asymModList,whichPnames = 'null_pNames',whichFun='null_fun')
      modList$stable_ending_asym <- stableAsymFit$par
      if(nrow(modList$varIn)>40){ ## fit stable asymptote to the last 10% (at least 4 obs)
        asymModList <- modList
        asymModList$varIn <- asymModList$varIn[order(asymModList$varIn[,'timeVar'])[round(nrow(asymModList$varIn)*.8):nrow(asymModList$varIn)],]
        stableAsymFit <- tef_tryFits(asymModList,whichPnames = 'null_pNames',whichFun='null_fun')
        modList$stable_ending_asym <- mean(c(modList$stable_ending_asym,stableAsymFit$par))
      }
      ## first, get rid of any other asym things:
      if(length(modList$pFix)>0){modList$pFix <- modList$pFix[grep('Asym',names(modList$pFix),invert = T)]}
      modList$pFix['pAsym'] <- round(modList$stable_ending_asym,8) ## set the asymtote to a fixed value, based on a stable fit to last bit
      modList <- tef_vars2forms(modList) ## re-get the model setup
    }else{cat('\nYou have too few observations to fit a stable asymptote to the final 20% of observations\n')}
  }
  modList$times['vars2forms'] <- Sys.time() - sum(modList$times)

  ## ## ## ## ## ## ## ## ##
  ## ## > FIT NULL MODEL

  # # CAN GET PARAMETER GUESSES FROM HERE
  ### ### ### CAN ALSO DO THINGS LIKE REDUCTION TO 2D SEARCH,
  # # # # NULL LL VALUES, NULL PREDICTED VALUES, ETC

  nullFit <- tef_tryFits(modList,whichPnames = 'null_pNames',whichFun='null_fun')

  parDat <- as.data.frame(matrix(nullFit$par[1:length(modList$null_pNames)],dim(modList$varIn)[1],length(modList$null_pNames),byrow=T))
  colnames(parDat) <- modList$null_pNames
  nullFit$fullDat <- data.frame(modList$varIn,parDat)

  modList$nullYhat <- eval(expr=modList$null_fun,env=nullFit$fullDat)

  modList$times['nullFits'] <- Sys.time() - sum(modList$times)

  ## ## ## ## ## ## ## ## ##
  ## ## > Fit Actual Model:
  bestFit <- tef_tryFits(modList)

  modList$times['modFits'] <- Sys.time() - sum(modList$times)

  if(modList$stepwise_asym){bestFit$par['stepwiseAsym'] <- modList$stable_ending_asym}

  ##
  ##
  if(modList$linkFun$link=='logit'||modList$linkFun$link=='weibull'){
    modList <- tef_getBounds(modList,linkFunX=modList$linkFun[[grep('X',names(modList$linkFun))]])}else{
      modList <- tef_getBounds(modList)
    }

  ##

  nObs <- length(na.omit(modList$varIn[,1]))

  ## ## ## ## ## ## ## ## ##
  ## ## >Get goodness-of-fit metrics
  bestFit$GoF <- data.frame(
    err = bestFit$value
    ,nullErr = as.numeric(nullFit$value)
    ,nPars = length(bestFit$par)
    ,nObs = nObs
  )
  rownames(bestFit$GoF) <- modList$errFun

  if(modList$errFun == 'ols'){
    # get F test here
    bestFit$GoF$Fval <-
      ((bestFit$GoF$nullErr-bestFit$GoF$err)/(bestFit$GoF$nPars-length(modList$null_pNames)))/
      (bestFit$GoF$err/(bestFit$GoF$nObs-bestFit$GoF$nPars))
    bestFit$GoF$Pval <- 1-pf(bestFit$GoF$Fval,
                             bestFit$GoF$nPars-1,
                             bestFit$GoF$nObs-bestFit$GoF$nPars)
    bestFit$GoF$Rsquared <- (bestFit$GoF$nullErr-bestFit$GoF$err)/bestFit$GoF$nullErr
    ## ## ## ##
    bestFit$GoF$BIC <- log(bestFit$value/nObs)*nObs + bestFit$GoF$nPars*log(nObs)
    bestFit$GoF$nullBIC <- log(nullFit$value/nObs)*nObs + length(nullFit$par)*log(nObs)
    bestFit$GoF$deltaBIC <- bestFit$GoF$BIC - bestFit$GoF$nullBIC

    ## >> next step is BIC-BF approximation: log(exp(deltaBIC/2),base=desired_base)
  }

  ## Need to switch() this

  if(modList$errFun == 'rmse'){
    bestFit$GoF$BIC <- log(bestFit$value^2)*nObs + bestFit$GoF$nPars*log(nObs)
    bestFit$GoF$nullBIC <- log(nullFit$value^2)*nObs + length(nullFit$par)*log(nObs)
    bestFit$GoF$deltaBIC <- bestFit$GoF$BIC - bestFit$GoF$nullBIC
  }
  if(modList$errFun == 'bernoulli'){
    bestFit$GoF$BIC <- bestFit$value*2 + bestFit$GoF$nPars*log(nObs)
    bestFit$GoF$nullBIC <- nullFit$value*2 + length(nullFit$par)*log(nObs)
    bestFit$GoF$deltaBIC <- bestFit$GoF$BIC - bestFit$GoF$nullBIC
  }
  if(modList$errFun == 'wiener_dr'){
    bestFit$GoF$BIC <- bestFit$value*2 + bestFit$GoF$nPars*log(nObs)
    bestFit$GoF$nullBIC <- nullFit$value*2 + length(nullFit$par)*log(nObs)
    bestFit$GoF$deltaBIC <- bestFit$GoF$BIC - bestFit$GoF$nullBIC
  }
  if(modList$errFun=='exGauss_mu' || modList$errFun=='exGauss_tau'){
    bestFit$GoF$BIC <- bestFit$value + bestFit$GoF$nPars*log(nObs)
    bestFit$GoF$nullBIC <- nullFit$value + length(nullFit$par)*log(nObs)
    bestFit$GoF$deltaBIC <- bestFit$GoF$BIC - bestFit$GoF$nullBIC
  }
  ## ^^ ^^ ^^ ^^ ^^ ^^ ##

  ## ## ## ## ## ## ## ## ##
  ## ## > Run resampled fits
  if(modList$bootPars$nBoots > 0){
    bootList <- tef_bootFits(modList)
    if(bootList$bootPercent<1){
      oosErrMedian <- median(bootList$bootFits$err_oos_mean)
      if(modList$errFun == 'rmse'){     ## this is already normalized for nObs
        bestFit$GoF$oosErr <- oosErrMedian
      }else{        ## this needs to be normalized for nObs
        bestFit$GoF$oosErr <- oosErrMedian*nObs
      }
      # bestFit$GoF$oosBIC <- oosErr*2 + length(modList$pNames)*log(nObs)
      bestFit$GoF$oosDeltaErr <- bestFit$GoF$oosErr-bestFit$GoF$nullErr
    }
    modList$times['bootFits'] <- Sys.time() - sum(modList$times)
  } ## ## ##
  ## ^^ ^^ ^^ ^^ ^^ ^^ ##

  ## ## ## ## ## ## ## ## ##
  ## everything beyond here is getting fit vals, plots, etc.
  {
    parDat <- as.data.frame(matrix(bestFit$par[1:length(modList$pNames)],dim(modList$varIn)[1],length(modList$pNames),byrow=T))
    colnames(parDat) <- modList$pNames
    bestFit$fullDat <- data.frame(modList$varIn,parDat)

    bestFit$fitVals <- eval(expr=modList$evalFun,env=bestFit$fullDat)

    if(modList$errFun=='exGauss_tau'){
      bestFit$fitVals <- bestFit$par['mu_param'] + (bestFit$fitVals)
    }

    if(modList$errFun=='exGauss_mu'){
      bestFit$fitVals <- bestFit$fitVals + (bestFit$par['tau_param'])
    }

    if(modList$changeFun=='power4'){
      timewarp <- bestFit$fullDat
      timewarp[,2] <- timewarp[,2]-bestFit$par[grep('PrevTime',names(bestFit$par))]
      timewarpPred <- eval(expr=modList$evalFun,env=timewarp)
      bestFit$learning_start_value <- timewarpPred[which.min(modList$varIn[,2])]
    }

    ## ## ## Get residuals
    bestFit$model_residuals <- bestFit$fitVals -  modList$varIn[,modList$respVar]
    modList$null_residuals  <- modList$nullYhat-  modList$varIn[,modList$respVar]

    ## <><> PROBLEMATIC FOR WEIBULL AND LOGIT -- NEED TO JUST HAVE A SEPARATE TRACK FOR THEM.

    ## ## residual vs raw correlation:
    bestFit$conditional_independence <- data.frame(
      rawSpearman = cor(modList$null_residuals,modList$varIn[,modList$timeVar],use='complete.obs',method='spearman'),
      modelConditionalSpearman = cor(bestFit$model_residuals,modList$varIn[,modList$timeVar],use='complete.obs',method='spearman')
    )
    bestFit$conditional_independence$proportionalSpearmanChange <-
      abs(bestFit$conditional_independence$modelConditionalSpearman)/abs(bestFit$conditional_independence$rawSpearman)
    ## ## if they have the psych package, then get the difference in correlations p value:
    try({
      bestFit$conditional_independence$pValSpearmanChange <-
        psych::r.test(nrow(na.omit(modList$varIn)),
                      abs(bestFit$conditional_independence$rawSpearman),
                      abs(bestFit$conditional_independence$modelConditionalSpearman))$p

      if(abs(bestFit$conditional_independence$rawSpearman) < abs(bestFit$conditional_independence$modelConditionalSpearman)){
        bestFit$conditional_independence$pValSpearmanChange <- max(c(
          1-bestFit$conditional_independence$pValSpearmanChange,
          bestFit$conditional_independence$pValSpearmanChange
        ))
      }

    },silent=T)

    ## ## if they have the tseries package, then get the null and model residuals' KPSS p values:
    try({
      suppressWarnings({
        null_kpss_p <- tseries::kpss.test(na.omit(modList$null_residuals)[sort(na.omit(modList$varIn[,modList$timeVar]))]
                                          ,null='Level')$p.value
        modl_kpss_p <- tseries::kpss.test(na.omit(bestFit$model_residuals)[sort(na.omit(modList$varIn[,modList$timeVar]))]
                                          ,null='Level')$p.value
      })

      if(null_kpss_p == .01){null_kpss_p <- '< .01'}
      if(modl_kpss_p == .01){modl_kpss_p <- '< .01'}
      if(null_kpss_p == .1){null_kpss_p <- '> .1'}
      if(modl_kpss_p == .1){modl_kpss_p <- '> .1'}

      bestFit$conditional_independence$pval_KPSS_null  <- null_kpss_p
      bestFit$conditional_independence$pval_KPSS_model <- modl_kpss_p

    },silent=T)
    rownames(bestFit$conditional_independence) <- paste0(modList$respVar,' ~ ',modList$timeVar,':')

    ## <><> for sigmoid links, find the threshold
    if(modList$linkFun$link=='logit' || modList$linkFun$link=='weibull'){try({
      bestFit$fitThresh <- eval(as.formula(paste('~',modList$thresh_fun))[[2]],env=bestFit$fullDat)

      if(modList$changeFun=='power4'){
        threshwarp <- eval(as.formula(paste('~',modList$thresh_fun))[[2]],env=timewarp)
        # print(threshwarp)
        bestFit$learning_start_thresh <- threshwarp[which.min(modList$varIn[,2])]
      }


      if(modList$bootPars$nBoots > 0){
        bootList$bootThresh <- data.frame()
        for(curBoot in 1:nrow(bootList$bootFits)){

          parDat <- as.data.frame(matrix(unlist(bootList$bootFits[curBoot,1:length(modList$pNames)]),
                                         dim(modList$varIn)[1],length(modList$pNames),byrow=T))
          colnames(parDat) <- modList$pNames
          curDat <- data.frame(modList$varIn,parDat)

          bootList$bootThresh <- rbind(bootList$bootThresh,eval(as.formula(paste('~',modList$thresh_fun))[[2]],env=curDat))
        }
        bootList$threshCI = list(ci025=apply(bootList$bootThresh,2,quantile,.025),
                                 ci975= apply(bootList$bootThresh,2,quantile,.975))
      }
    },silent=T)}

    if(dim(modList$varIn)[2]>2){
      noCovar <- modList$varIn ; noCovar[,3:dim(noCovar)[2]] <- 0
      noCovar[,modList$blockTimeVar] <- 1E10
      bestFit$fitVals_noCovar <- eval(expr=modList$evalFun,env=data.frame(noCovar,parDat))
    }

    modList$times['postFit'] <- Sys.time() - sum(modList$times)

    modList$times['totalTime'] <- Sys.time() - modList$times[1]

    if(modList$bootPars$nBoots > 0){
      modList$times['mean_per_boot'] <- modList$times['bootFits']/modList$bootPars$nBoots
    }
  } ## ##
  ## ^^ ^^ ^^ ^^ ^^ ^^ ##

  ## ## ## ## ## ## ## ## ##
  ## Wrap things up and return them
  modList$times <- round(modList$times[2:length(modList$times)],4)
  outData <- data.frame(bestFit$fullDat,fitVals=bestFit$fitVals)

  if(exists('bootList')){
    TEs3 <-list(modList=modList,model=bestFit,nullFit=nullFit,data=outData,bootList=bootList)
  }else{
    TEs3 <- list(modList=modList,model=bestFit,nullFit=nullFit,data=outData)
  }

  class(TEs3) <- 'TEfit'

  return(TEs3)

}
