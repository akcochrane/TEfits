
#' Get help for the TEfits package
#'
#' Many options for fitting using TEfits are specified in obscure lists and such. To help,
#' this function provides verbose explanations of how to use various tools.
#'
#' @param topic Help topic.
#'
#' @export
#' @examples
#' TEhelp('package')
#' TEhelp('errFun')
#' TEhelp('linkFun=d_prime')
#' TEhelp('errFun=logcosh')
#' TEhelp('summary(TEfit)')
#'
TEhelp <- function(topic='package'){
  if(topic=='package'){cat(
    'This package is designed to facilitate painless nonlinear regression of time-evolving measures.
Every effort is made to balance sensible defaults, full customization, and interpretable parameterizations.

    Use this help function to get more information about specific aspects. See ?TEhelp.'
  )}

  if(topic=='errFun'){cat(
    "Various functions are available to optimize."
    ,'These include:\nSum of squared error (`ols`; the default),'
    ,'\nRoot mean squared error (`rmse`),'
    ,'\nBernoulli (binary binomial; `bernoulli`),'
    ,'\nlog-hyperbolic-cosine (`logcosh`).

    These are specified as an argument to TEfit(..., errFun="exGauss_tau", ...), TEfitAll(..., errFun="logcosh", ...), etc.
    ___ ___ ___ ___'
  )}

  if(topic=='linkFun=logistic'){cat('help page pending')}

  if(topic=='linkFun=d_prime'){cat('With the d_prime link function, you can use a binary (0 or 1) variable called `presence` to
                                   categorize your outcomes (bounded at 0 and 1) as being the result of a "present" event (Miss or Hit)
                                   or an "absent" event (Correct Rejection or False Alarm). This link function first uses
                                   TEfits::tef_acc2dprime to calculate a bounded running d_prime, then fits that running d_prime as
                                   the response variable (defaulting to OLS fitting, |d_prime| bounded at 5, and a smoothing HWHM of 3.

                                   Example:
                                   mod_dprime <- TEfit(dat[,c("acc,"trialNum","present")],linkFun = list(link="d_prime",presence="present",smooth_hwhm=2))
                                   __ __ __ __')}

  if(topic=='errFun=logcosh'){cat('The log-hyperbolic-cosine error function'
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
  )}

  if(topic=='errFun=ols'){cat('help page pending')}
  if(topic=='errFun=bernoulli'){cat('help page pending')}
  if(topic=='errFun=rmse'){cat('help page pending')}
  if(topic=='errFun=exGauss_tau'){cat('help page pending')}
  if(topic=='errFun=exGauss_mu'){cat('help page pending')}

  if(topic=='cross-validate'){cat('help page pending')}
  if(topic=='bootstrap'){cat('help page pending')}

  if(topic=='summary(TEfit)'){cat('The summary function for a standard
                                  time-evolving model is intended to convey several pieces of information.
                                  Of greatest importance are the actual values of fit parameters. Additionally,
                                  several fit and convergence indices may assist with model interpretation.
                                  BIC and delta-BIC relative to a non-time-evolving model may be utilized to compare
                                  the relative benefits of additional [time-evolving] parameters. Likewise, the
                                  nonindependence between response variables and time (and the change thereof) is
                                  reported.')}




  if(topic=='summary(TEfitAll)'){cat('help page pending')}

  if(topic=='control(explicit)'){cat('The most flexibility in nonlinear model fitting is implemented when
                                     an entry into the control list is "explicit". As with other nonlinear
                                     fitting methods (e.g., nls(), nlmer(), brm(), this requires a full and
                                     specific parameterization of the regression formula. All terms in the
                                     formula that do not exactly match input variables are assumed to be
                                     parameters to be estimated.')}

  if(topic=='e'){cat('help page pending')}
}
