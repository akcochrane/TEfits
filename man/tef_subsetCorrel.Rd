% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tef_subsetCorrel.R
\name{tef_subsetCorrel}
\alias{tef_subsetCorrel}
\title{Find mean correlations of subsets of vectors}
\usage{
tef_subsetCorrel(
  x,
  y,
  method = "spearman",
  returnVal = "mean",
  subsample = "even_odd",
  iter = 200
)
}
\arguments{
\item{x}{vector to correlate}

\item{y}{vector to correlate}

\item{method}{correlation method (e.g., "spearman" or "pearson")}

\item{returnVal}{return "mean" or (if subsample is random) "quantiles"}

\item{subsample}{should the subsamples be "even_odd" or "random" (half of the first half, half of the second half)}

\item{iter}{if subsample is "random", number of times the vectors are split, each half is subset, those subsets are correlated, and those correlations are averaged}
}
\description{
Selects subsets of x and y vectors,
and calculates the mean correlations between the two subsets.
}
