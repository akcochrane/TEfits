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
#' @param changePar  Character. Which variable, "threshold" or "bias", changes over time. The other one is stable over time; \emph{the stable component inherits its formula from the asymptote component}.
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

  ##ISSUE## Make bias inherit from the null model, not asymptote

  ##ISSUE## don't paste `attr(changeStr,'formula')` into the right hand side? At least, not to start.
  # # # # for TEbrm, need to have it be modular.... passing in the "clean" link and change functions, and
  # # # # having the LHS of the change function be one of the components of the link function

  threshBase <- threshVal/(1-threshVal)

  changeStr <- eval(changeForm)

  if(length(changePar)>1){changePar <- changePar[1]}
  if(changePar == 'threshold'){
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
                                         'bias)-',linkX,')/exp(',
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
                    changeStr,')-',linkX,')/exp(',
                    'logThreshold)))'
    )
    ## formula for TEbrm
    attr(rhs,'formula') <- paste0(lapseRate,' + (1-2*',lapseRate,')/(1+',threshBase,'^(((',
                                  attr(changeStr,'formula'),'-',linkX,')/exp(',
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
                                       attr(rhs,'nullFun') ,')-',linkX,')/exp(',
                                       'logThreshold)))'
    )

  }else(stop('Please identify changePar as threshold or bias within tef_link_logistic()'))

  ## ## NEED TO DOUBLE AND TRIPLE CHECK, TO BE APPROPRIATE FOR THE LINK
  ## STILL NEED TO WORK WITH PARAMETER BOUNDARIES FOR THE THRESH / BIAS

  try({
    attr(rhs,'constantPar_prior') <- brms::set_prior(constantPar_prior, nlpar = attr(rhs,'constantPar'))
  },silent=T)
  attr(rhs,'link_start_asym') <- 'exp'
  attr(rhs,'boundScale') <- boundScale
  attr(rhs,'linkX') <- linkX
  attr(rhs,'threshVal') <- threshVal

  attr(rhs,'link_explanation') <- paste('The link function is logistic. This means that a variable "',linkX
                                        ,'" defines the [inverse-logit] interpolation of predicted values between'
                                        ,'zero and one (with a small lapse rate included). In psychometric functions'
                                        ,'this is often a "stimulus strength" variable that is related to one of'
                                        ,'two responses. The link function then allows the relationship between variables'
                                        ,'to be estimated in terms of a location [bias] and scale [threshold] parameter,'
                                        ,'with the threshold parameter usually being of primary interest. The threshold parameter'
                                        ,'can be interpreted as "the magnitude of',linkX,'that will produce'
                                        ,threshVal,'percent `1` responses and',1-threshVal,'percent `0` responses."'
                                        ,'Within TEfits, then, change over time is usually considered in the threshold'
                                        ,'parameter, although change in the bias parameter can also be fit. For further'
                                        ,'reading on time-changing psychometric functions see `Kattner, Cochrane,'
                                        ,'& Green, (2017) Journal of Vision, https://doi.org/10.1167/17.11.3`')

  return(rhs)

}
