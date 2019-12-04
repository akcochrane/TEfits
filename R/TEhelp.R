
#' Get help for the TEfits package
#'
#' @param topic Help topic. options are "package", "errFun"
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
  if(topic=='linkFun=d_prime'){cat('help page pending')}

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
}
