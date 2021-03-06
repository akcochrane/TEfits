% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tef_checkPars.R
\name{tef_checkPars}
\alias{tef_checkPars}
\title{Check for bound hitting and other undesireable outcomes within an optim() call}
\usage{
tef_checkPars(
  err,
  guesses,
  curDat,
  pNames,
  evalFun,
  errFun,
  respVar,
  linkFunX = NA,
  y_lim,
  rate_lim,
  shape_lim,
  penalizeRate,
  paramTerms,
  guessGroups = NULL
)
}
\arguments{
\item{err}{Error}

\item{guesses}{Parameter values}

\item{curDat}{Data being fit}

\item{pNames}{Parameter names}

\item{evalFun}{Function being fit}

\item{errFun}{Function to calculate error}

\item{respVar}{Name of the response variable}

\item{linkFunX}{If relevant, the "x" value for a link function (e.g., Weibull, logistic)}

\item{y_lim}{Limits to fit values}

\item{rate_lim}{Limits to rate parameter}

\item{shape_lim}{If using a Weibull change function, limits to Weibull shape parameter}

\item{penalizeRate}{Logical. Should error be penalized if rate is extremely close to the bounds?}

\item{paramTerms}{parameter-level regressions, to be evaluated for checking y_lim and rate_lim}

\item{guessGroups}{deprecated}
}
\description{
TEfits internal.
}
\details{
Sane boundaries for parameters are the only way that many nonlinear regression optimizations
can be identifiable. Fortunately, theory-driven constraints on parameter ranges provide useful
a priori restrictions on the possible ranges for parameters and model predictions.
This function checks the following:
 \itemize{
\item{\code{start and asymptote parameters} -- all models are parameterized in terms of
starting and ending values. This ensures that the starting and ending values comply with
the \emph{y_lim} boundaries; \emph{y_lim} may be user-defined, defined by another model feature (e.g.,
\emph{bernoulli} error function is limited to predicted values of 0 or 1; Weibull link thresholds must be
above 0).}
\item{\code{rate parameter} -- If not user-input, then defined by \code{TEfits::tef_getLinkedFun}.
Defaults, with exponential change, to a minimum that would provide 50% of model change in \code{sd(timeVar[1:10])}
amount of time, and to a maximum value that would provide 80% of model change at \code{max(timeVar)}. Other change
functions have limits that, with their respective parameterizations, are intended to imitate the limits of the
3-parameter exponential (i.e., imitate the overall shape of the curve's extremes) \strong{These are
heuristics.} The default values are intended to be flexible while maintaining a sufficiently constrained curve such that
\emph{both} starting asymptote parameters are interpretable (i.e., if the rate parameter, which is a time constant,
were to be extremely small, the start parameter could become infinitely large or small). If a time-evolving process
occurs on a timescale that cannot be fit by the default boundaries, it is likely that the data is unsufficient to
characterize that process.}
\item{\code{pPrevTime parameter} -- 4-parameter Power change utilizes a so-called "previous learning time" parameter
that assumes that the learning function extents \emph{backward} through time. This parameter must be greater than
0 and smaller than 1*10^5}
}

Users are highly encouraged to use their own boundaries (e.g., \code{y_lim} & \code{rate_lim}), given knowledge of a specific dataset, using \code{\link{tef_control}}.
}
