% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tef_runningMean.R
\name{tef_runningMean}
\alias{tef_runningMean}
\title{1-dimensional Gaussian smoothing}
\usage{
tef_runningMean(x, k_hwhm = 2, distr = "gaussian")
}
\arguments{
\item{x}{vector to be smoothed}

\item{k_hwhm}{The half-width half-max of the kernal [when Gaussian = sd*1.17741]. This is the index distance at which an element receives half the weight of the element at the center of the smooothing.}

\item{distr}{The distribition's density function to be used. 'gaussian' or 'cauchy'}
}
\description{
Runs a Gaussian or Cauchy density estimate centered over each
element [numeric, logical, or NA]
of the vector, and calculates
a density-weighted average for that element's index.
}
