#' Construct a weibull link function parameterized with threshold and bias
#'
#' Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' \strong{\code{shape}} is a parameter in the weibull function, and \strong{must not} be
#' used as a name of data variables (e.g., \code{linkX} or within \code{changeForm}).
#'
#' @param changeForm The formula describing the change in threshold
#' @param linkX      Character. The name of the "x" variable in the weibull link function (e.g., stimulus strength in a psychometric function). Should be a positive real numeric variable (e.g., presentation time or number of targets).
#' @param threshVal  The threshold at which to evaluate the weibull function (i.e., the y-value for which threshold describes the x-value).
#' @param rhAsymptote The asymptotic value of the weibull function with large \code{linkX} values (e.g., accuracy at infinitely large stimulus strength).
#' @param yIntercept The origin value of the weibull function (with a \code{linkX} value of zero, e.g., accuracy at a stimulus strength of zero; in behavioral data is likely to be "guessing rate").
#' @param lapseRate The offset, from rhAsymptote, of the weibull function at arbitrarily large values of \code{linkX}. A small lapse rate improves model fit (see Wichmann and Hill, 2001, P&P).
#' @param boundScale Currently not implemented. Upper threshold of threshold estimates, as a multiple of the maximum absolute \code{linkX}.
#' @param constantPar_prior The prior to put on the constant component of the weibull function (i.e., shape parameter). Only relevant if passing the model to a \code{\link[brms]{brm}} model (e.g., with \code{\link{TEbrm}}).
#'
#' @export
#'
#' @examples
#'
#' equation_to_fit <- tef_link_weibull( tef_change_expo3('trialNum', parForm = ~ (1|subID)) , linkX = 'absoluteRatio' )
#'
tef_link_weibull <- function(changeForm,
                              linkX,
                              threshVal = .75,
                              rhAsymptote = 1,
                              yIntercept = .5,
                              lapseRate = .005,
                              boundScale = 2,
                              constantPar_prior = 'normal(0,3)'){

  ##ISSUE## Make bias inherit from the null model, not asymptote
  ## to do: insert Weibull rather than logistic, and change relevant arguments

  # find the base (for the exponent) that gives the appropriate threshold
  expBase <- round(
    uniroot(function(expBase) threshVal-(yIntercept+((rhAsymptote-yIntercept)-lapseRate)*(1-expBase^(-1))),c(.0001,1000))$root
    ,4)

  changeStr <- eval(changeForm)

    rhs <- paste0(
           yIntercept,'+((',rhAsymptote,'-',yIntercept,')-',lapseRate,')*(1-',
           expBase,'^(-(',linkX,'/(',
           changeStr,'))^shape))'

    )

    {
      attributes(rhs) <- attributes(changeStr)
      attr(rhs,'changePar') <- 'threshold'
      attr(rhs,'constantPar') <- 'shape'
      attr(rhs,'parForm')[['shape']] <- attr(rhs,'parForm')$pAsym
      attr(attr(rhs,'parForm')[['shape']],'parameters') <- '' ##ISSUE## may need to include something here
      attr(attr(rhs,'parForm')[['shape']],'equation') <- '' ##ISSUE## may need to include something here


      attr(rhs,'allPars') <- c(attr(rhs,'shape'),shape = 'shape')
      attr(rhs,'nullForm')  <-    paste0(
                                         yIntercept,'+((',rhAsymptote,'-',yIntercept,')-',lapseRate,')*(1-',
                                         expBase,'^(-(',linkX,'/(',
                                         attr(rhs,'nullFun'),'))^shape))'

      )

      ## formula for TEbrm
      attr(rhs,'formula') <- paste0(
             yIntercept,'+((',rhAsymptote,'-',yIntercept,')-',lapseRate,')*(1-',
             expBase,'^(-(',linkX,'/threshold)^(',
             expBase,'^shape))) , parameter_threshold ~ ',attr(changeStr,'formula')

      )


    } # collapse the attributes for the thresh


  ## ## NEED TO DOUBLE AND TRIPLE CHECK, TO BE APPROPRIATE FOR THE LINK
  ## STILL NEED TO WORK WITH PARAMETER BOUNDARIES FOR THE THRESH / BIAS

  try({
    attr(rhs,'constantPar_prior') <- brms::set_prior(constantPar_prior, nlpar = attr(rhs,'constantPar'))
  },silent=T)
  attr(rhs,'link_start_asym') <- 'exp'
  attr(rhs,'boundScale') <- boundScale
  attr(rhs,'linkX') <- linkX
  attr(rhs,'threshVal') <- threshVal

  return(rhs)

}
