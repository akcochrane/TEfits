
#' Get help for the TEfits package
#'
#' @param topic Help topic. To see options, see TEhelp('package')
#'
#' @export
#'
TEhelp <- function(topic='package'){
  if(topic=='package'){cat(
    'This package is designed to facilitate painless nonlinear regression of time-evolving measures.'
        ,'Use this help function to get more information about specific aspects. \n'
    ,'\nOptions:\nTEhelp("errFun")'
    ,'\nTEhelp("errFun=logcosh")'
    )}

  if(topic=='errFun'){cat(
    "Various functions are available to optimize."
    ,'These include:\nSum of squared error (`ols`; the default),'
      ,'\nRoot mean squared error (`rmse`),'
    ,'\nBernoulli (binary binomial; `bernoulli`),'
    ,'\nlog-hyperbolic-cosine (`logcosh`),'
  )}

  if(topic=='linkFun=logistic'){cat('help page pending')}
  if(topic=='linkFun=d_prime'){cat('With the d_prime link function, you can use a binary (0 or 1) variable called `presence` to
                                   categorize your outcomes (bounded at 0 and 1) as being the result of a "present" event (miss or hit)
                                   or an "absent" event (Correct Rejection or False Alarm). This link function first uses
                                   TEfits::tef_acc2dprime to calculate a bounded running d_prime, then fits that running d_prime as
                                   the response variable (defaulting to OLS fitting, |d_prime| bounded at 5, and a smoothing HWHM of 3.')}

  if(topic=='errFun=logcosh'){cat('The log-hyperbolic-cosine error function'
                                  ,'is a compromise between OLS and absolute error'
                                  ,'\nfunctions. While at absolute errors of approximately'
                                  ,'4 or greater the function is '
                                  ,"\nindistinguishable from absolute error, at smaller errors the function's "
                                  ,'slope decreases,'
                                  ,'\nwhich allows for easier optimization. In practice,'
                                  ,'this is a versatile error function that'
                                  ,'\nis especially well-suited for data with outliers or other shapes'
                                  ,'indicating\ndispersion beyond a standard Gaussian distribution.'
                                  ,'\n\nImplementation: log(cosh(y-yHat))\n__ __ __ __'
                                  )}

  if(topic=='errFun=ols'){cat('help page pending')}
  if(topic=='errFun=bernoulli'){cat('help page pending')}
  if(topic=='errFun=rmse'){cat('help page pending')}
  if(topic=='errFun=exGauss_tau'){cat('help page pending')}
  if(topic=='errFun=exGauss_mu'){cat('help page pending')}

  if(topic=='e'){cat('help page pending')}
}
