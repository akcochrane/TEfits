% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tef_change_gam.R
\name{tef_change_gam}
\alias{tef_change_gam}
\title{Construct a by-time Generalized Additive Model formula}
\usage{
tef_change_gam(
  timeVar,
  timeCovar = c(NULL),
  groupingVar = "",
  bs = c("ts", "cr", "cs", "tp", "cc")
)
}
\arguments{
\item{timeVar}{String. Indicates the name of the time variable}

\item{timeCovar}{Vector of strings, for variables that will be included in the time-evolving model.}

\item{groupingVar}{String. Optional grouping variable (e.g., participant ID, in a behavioral study). If this is non-empty, then all \code{timeCovar} are fit on the level of \code{groupingVar}}

\item{bs}{Two letter character string inidicating the basis for smoothing. See ?mgcv::s}
}
\description{
By defining the model variable associated with time (e.g., trial number), and
formulas defining each of the nonlinear parameters of time-related change,
this function constructs a model that can then be passed to functions for fitting
the model (e.g., \code{\link{TEbrm}}).
}
\details{
Use is primarily for passing to link functions. If no link function is needed, then an equivalent
formula can be passed directly to \code{\link[brms]{brm}}.

Function is \strong{under development}
and is likely to be buggy, and to change frequently.
}
\examples{
equation_to_fit <- tef_change_gam('trialNum')


}
\seealso{
\code{\link{TEbrm}} for examples of how to use this function in specifying models.
}
