
#' Get help for the TEfits package
#'
#' Many options for fitting using TEfits are specified in a variety of places. To help,
#' this function provides verbose explanations of how to use various tools. Topics will be
#' gradually added, with no guarantee that a given topic has an entry yet.
#'
#' @param topic Help topic.
#'
#' @export
#' @examples
#' ## Brief package summary:
#' TEhelp('package')
#'
#' ## List topics with TEhelp entries:
#' TEhelp('topics')
#'
#' ## Overview of error functions:
#' TEhelp('errFun')
#'
#' ## The log-cosh error function:
#' TEhelp('errFun=logcosh')
#'
#' ## The ex-Gaussian error function, with change in the exponential component:
#' TEhelp('errFun=exGauss_tau')
#'
#' ## The d prime link function:
#' TEhelp('linkFun=d_prime')
#'
#' ## The summary method for TEfit models:
#' TEhelp('summary(TEfit)')
#'
TEhelp <- function(topic='package'){

  helpTable <- data.frame(matrix(NA,0,2))
  colnames(helpTable) <- c('topic','description')

  helpTable <-rbind(helpTable,data.frame(topic='package',description=paste0(
    'This package is designed to facilitate painless nonlinear regression of time-evolving measures.
Every effort is made to balance sensible defaults, full customization, and interpretable parameterizations.

For more information see ?TEfits, the README at github.com/akcochrane/TEfits, and/or vignette("TEfit_tutorial")

Use this help function to get more information about specific aspects. See ?TEhelp.'
  ),stringsAsFactors = TRUE))


  ## ## ##
  ## error functions
  {
    helpTable <-rbind(helpTable,data.frame(topic='errFun',description=paste0(
      "Various functions are available to optimize."
      ,'These include:\nSum of squared error (`ols`; the default),'
      ,'\nRoot mean squared error (`rmse`),'
      ,'\nBernoulli (binary binomial; `bernoulli`),'
      ,'\nlog-hyperbolic-cosine (`logcosh`).

    These are specified as an argument to TEfit(..., errFun="exGauss_tau", ...), TEfitAll(..., errFun="logcosh", ...), etc.
    ___ ___ ___ ___'
    )))

    helpTable <-rbind(helpTable,data.frame(topic='errFun=logcosh',description=paste0(
      'The log-hyperbolic-cosine error function'
      ,'is, conceptually, a compromise between OLS and absolute error'
      ,'\nfunctions. While at absolute errors of approximately'
      ,'4 or greater the function is functionally'
      ,"\nindistinguishable from absolute error, at smaller errors the function's"
      ,'slope decreases,'
      ,'\nwhich allows for easier optimization. In practice,'
      ,'this is a versatile error function that'
      ,'\nis especially well-suited for data with outliers or other shapes'
      ,'indicating\ndispersion beyond a standard Gaussian distribution.'
      ,'\n\nExample: mod <- TEfit(dat[,c("resp","trialNum")],errFun="logCosh")'
      ,'\n\nR Implementation: log(cosh(y-yHat))\n__ __ __ __'
    )))

    if(topic=='errFun=ols'){cat('help page pending. ')}
    if(topic=='errFun=bernoulli'){cat('help page pending. ')}
    if(topic=='errFun=rmse'){cat('help page pending. ')}
    helpTable <-rbind(helpTable,data.frame(topic='errFun=exGauss_tau',description=paste0(
      'The ex-Gaussian distribution is a 3-parameter convolution of exponential
       and Gaussian distributions often used to fit response time data.
       By fitting change in the tau [exponential mean] component of this
       distribution, the frequent covariance between mean and variance of RT
       is naturally accommodated.'
    )))

    if(topic=='errFun=exGauss_mu'){cat('help page pending. ')}
  } # close error functions ##
  ## ## ##
  ## link functions
  {
    if(topic=='linkFun=logistic'){cat('help page pending. ')}

      helpTable <-rbind(helpTable,data.frame(topic='linkFun=d_prime',description=paste0(
'With the d_prime link function, you can use a binary (0 or 1) variable called `presence` to
categorize your outcomes (bounded at 0 and 1) as being the result of a "present" event (Miss or Hit)
or an "absent" event (Correct Rejection or False Alarm). This link function first uses
TEfits::tef_acc2dprime to calculate a bounded running d_prime, then fits that running d_prime as
the response variable (defaulting to OLS fitting, |d_prime| bounded at 5, and a smoothing HWHM of 3.

Example:
mod_dprime <- TEfit(dat[,c("acc,"trialNum","present")],linkFun = list(link="d_prime",presence="present",smooth_hwhm=2))
__ __ __ __'
)))
  } # close link functions
  ## ## ##
  ## change functions
  {
    helpTable <-rbind(helpTable,data.frame(topic='changeFun=expo',description=paste0(
'The default 3-parameter exponential change function.
Parameters represent the start, rate, and asymptotic values. Rate is log2(time taken to complete 50% of change).
Implementation: pAsym + (pStart - pAsym) * 2^((min(timeVariable) - timeVariable)/2^pRate)
pAsym, pStart, and pRate may each be predicted by some combination of covariates.
__ __ __ __'
)))
    helpTable <-rbind(helpTable,data.frame(topic='changeFun=power',description=paste0(
'3-parameter power change function.
Parameters represent the start, rate, and asymptotic values. Rate parameterized such that the value of rate is on
a similar scale as rates used in the default 3-parameter exponential function.
Implementation: pAsym + (pStart - pAsym) * (timeVariable- (min(timeVariable)-1))^(log(0.25)/log(2^pRate))
pAsym, pStart, and pRate may each be predicted by some combination of covariates.
__ __ __ __'
)))
  } # close change functions

  ## ## ##
  ## summary functions
  {
    helpTable <-rbind(helpTable,data.frame(topic='summary(TEfit)',description=paste0(
'The summary function for a standard
                                  time-evolving model is intended to convey several pieces of information.
                                  Of greatest importance are the actual values of fit parameters. Additionally,
                                  several fit and convergence indices may assist with model interpretation.
                                  BIC and delta-BIC relative to a non-time-evolving model may be utilized to compare
                                  the relative benefits of additional [time-evolving] parameters. Likewise, the
                                  nonindependence between response variables and time (and the change thereof) is
                                  reported.'
)))
  if(topic=='summary(TEfitAll)'){cat('help page pending. ')}
  } # close summary functions
  ## ## ##
  ## control arguments
  {
helpTable <-rbind(helpTable,data.frame(topic='control(explicit)',description=paste0(
 'The most flexibility in nonlinear model fitting is implemented when
                                     an entry into the control list is "explicit". As with other nonlinear
                                     fitting methods (e.g., nls(), nlmer(), brm(), this requires a full and
                                     specific parameterization of the regression formula. All terms in the
                                     formula that do not exactly match input variables are assumed to be
                                     parameters to be estimated.'
)))
  } # close control arguments


  # others
  if(topic=='cross-validate'){cat('help page pending. ')}
  if(topic=='bootstrap'){cat('help page pending. ')}
  if(topic==''){cat('help page pending. ')}

  ## ## ##
  ## Put it all together
  helpTable <-rbind(helpTable,data.frame(topic='topics',description=paste0(
    "Current topics with TEhelp entries are:\n",paste(helpTable$topic,collapse = '\n'))))

  if(any(helpTable$topic == topic)){cat(levels(droplevels(helpTable[helpTable$topic==topic,'description'])))}else{
    cat("The topic does not have an entry in TEhelp. Please refer to ?TEhelp and/or TEhelp('topics').")
  }

}
