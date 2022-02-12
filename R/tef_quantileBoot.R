
#' Bootstrap quantiles
#'
#' Resamples a numeric vector with replacement, finds the requested quantiles
#' for each resample, and returns the trimmed average of the resampled quantiles.
#' See \code{\link[stats]{quantile}}.
#'
#' Returns the mean of the middle 5% of the resamples for each quantile (i.e., \code{mean(quantile_i,trim = .475)}). In
#' practice this means that the median will be returned if the number of resamples is small enough, and
#' that the default number of 1000 resamples will use the middle 50 resamples to compute the mean.
#'
#' @param x Numeric vector over which to resample and compute quantiles
#' @param probs Numeric vector of probabilities with values in \emph{[0,1]}.
#' @param nBoot Number of resamples-with-replacement
#' @param na.rm Logical; if true, any NA and NaN's are removed from \code{x} before the quantiles are computed.
#' @param ... Other arguments to pass to \code{quantile()}
#'
#' @export
#'
#' @examples
#' exp_quantiles <- tef_quantileBoot(c(rexp(100),NA))
#' cat(exp_quantiles)
#'
tef_quantileBoot <- function(x,probs  = c(.025, .5, .975), nBoot = 1000,na.rm=T ,...){

  # # TO DO: run some simulations and get absolute error and MAD vs normal quantiles, with different boot numbers

  # x <- rnorm(20) ; probs <- c(.025, .5, .975) ; nBoot = 1000  ; na.rm=T# for testing

  quantSamples <- replicate(nBoot,
                            {
                              quantile(
                                sample(x,length(x),replace = T)
                                ,probs = probs
                                ,na.rm=na.rm
                                ,...
                              )
                            }
  )

  quants <- apply(quantSamples,1,mean,trim = .475, na.rm=T)

  names(quants) <- names(quantile(x,probs=probs,na.rm=na.rm,...))

  attr(quants,'samples') <- quantSamples

  return(quants)

}


