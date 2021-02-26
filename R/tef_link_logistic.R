#' Construct a logistic link function parameterized with threshold and bias
#'
#' Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' If bias is changing, threshold inherits its formula from asymptotic bias.
#' If threshold is changing, bias inherits its formula from asymptotic threhold.
#'
#' @param changeForm The formula describing the change in either
#' @param linkX      Character. The name of the "x" variable in the logistic link function (e.g., stimulus strength in a psychometric function)
#' @param changePar  Character. Which variable, "threshold" or "bias", changes over time. The other one is stable over time.
#' @param threshVal  The threshold at which to evaluate the logistic function (i.e., the y-value for which threshold describes the x-value).
#' @param lapseRate  The offset, from 0 or 1, of the logistic function at arbitrarily large (positive or negative) values of \code{linkX}. A small lapse rate improves model fit (see Wichmann and Hill, 2001, P&P).
#' @param boundScale Currently not implemented. Upper threshold of threshold estimates, as a multiple of the maximum absolute \code{linkX}.
#' @param constantPar_prior The prior to put on the constant component of the logistic function (i.e., either the bias or the log threshold). Only relevant if passing the model to a \code{\link[brms]{brm}} model (e.g., with \code{\link{TEbrm}}).
#'
#' @export
#'
#' @examples
#'
#' equation_to_fit <- tef_link_logistic( tef_change_expo3('trialNum', parForm = ~ (1|subID)) , linkX = 'ratio' )
#'
tef_link_logistic <- function(changeForm,
                              linkX,
                              changePar = c('threshold','bias'),
                              threshVal = .75,
                              lapseRate = .005,
                              boundScale = 2,
                              constantPar_prior = 'normal(0,3)'){

  ##ISSUE## I'm getting some "Error evaluating the log probability at the initial value" when using bernoulli, which should
  ## never be the case with this link function. So, there may be something wrong.

  threshBase <- threshVal/(1-threshVal)

  changeStr <- eval(changeForm)

  if(changePar == 'threshold' || all(changePar == c('threshold','bias'))){
  rhs <- paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                                                       'bias)-',linkX,')/(',
                                                       changeStr,')))'
  )

  {
  attributes(rhs) <- attributes(changeStr)
  attr(rhs,'changePar') <- 'threshold'
  attr(rhs,'constantPar') <- 'bias'
  attr(rhs,'parForm')[['bias']] <- attr(rhs,'parForm')$pAsym
  attr(attr(rhs,'parForm')[['bias']],'parameters') <- '' ##ISSUE## may need to include something here
  attr(attr(rhs,'parForm')[['bias']],'equation') <- '' ##ISSUE## may need to include something here

  attr(rhs,'allPars') <- c(attr(rhs,'allPars'),bias = 'bias')
  attr(rhs,'nullForm')  <-    paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                                     'bias)-',linkX,')/log(',
                                     attr(rhs,'nullFun') ,')))'
  )

  ## formula for TEbrm
  attr(rhs,'formula') <- paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                                'bias)-',linkX,')/(',
                                attr(changeStr,'formula'),')))'
  )

  } # collapse the attributes for the thresh

  }else if(changePar == 'bias'){
    rhs <-   paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                    changeStr,')-',linkX,')/log(',
                    'logThreshold)))'
    )
    ## formula for TEbrm
    attr(rhs,'formula') <- paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                                  attr(changeStr,'formula'),'-',linkX,')/log(',
                                  'logThreshold)))'
    )

    attributes(rhs) <- attributes(changeStr)
    attr(rhs,'changePar') <- 'bias'
   attr(rhs,'constantPar') <- 'logThreshold'
    attr(rhs,'parForm')[['threshold']] <- attr(rhs,'parForm')$pAsym
    attr(attr(rhs,'parForm')[['threshold']],'parameters') <- '' ##ISSUE## may need to include something here
    attr(attr(rhs,'parForm')[['threshold']],'equation') <- '' ##ISSUE## may need to include something here


    attr(rhs,'allPars') <- c(attr(rhs,'allPars'),threshold = 'threshold')
    attr(rhs,'nullForm')  <-    paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                                       attr(rhs,'nullFun') ,')-',linkX,')/log(',
                                       'logThreshold)))'
    )

  }else(stop('Please identify changePar as threshold or bias within tef_link_logistic()'))

    ## ## NEED TO DOUBLE AND TRIPLE CHECK, TO BE APPROPRIATE FOR THE LINK
  ## STILL NEED TO WORK WITH PARAMETER BOUNDARIES FOR THE THRESH / BIAS

  try({
    attr(rhs,'constantPar_prior') <- brms::set_prior(constantPar_prior, nlpar = attr(rhs,'constantPar'))
  },silent=T)
  attr(rhs,'link_start_asym') <- 'log'
  attr(rhs,'boundScale') <- boundScale
  attr(rhs,'linkX') <- linkX

  return(rhs)

}
