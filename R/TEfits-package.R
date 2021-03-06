#' TEfits: Time-evolving model fits
#'
#' Data is described, interpreted, and tested using indices such as d prime,
#' mean, or psychometric function threshold. This package serves to allow the same
#' questions to be asked about time-evolving aspects of these indices, namely,
#' the starting level, the amount of time that the index takes to change, and the
#' asymptotic level of that index. Nonlinear regression applied to time-evolving
#' functions is made as intuitive and painless as is feasible, with many
#' extensions if desired.
#'
#' @section Time-evolving fits:
#'
#' The \code{\link{TEfit}} function is the primary user-oriented function of
#' the \code{TEfits} package. It allows for nonlinear fitting of time-related
#' change in an outcome variable by estimating that outcome variable's value at
#' each timepoint. See \code{\link{TEfit}}, \code{\link{TEfitAll}} for fitting
#' a \code{\link{TEfit}} model to subsets of data (e.g., individual participants),
#' and \code{vignette('TEfits_tutorial')} for an introduction to the framework.
#'
#' @section Nonlinear regressors in [generalized] linear models:
#'
#' While \code{\link{TEfit}} is intended to interrogate time-evolving trends
#' themselves within data, \code{TEfits} also includes several extensions to common
#' regression functions that allow for seamless incorporation of a nonlinear
#' [exponentially saturating] variable of time. These functions approach time-evolving
#' dynamics from a common perspective in behavioral research: Behavior "of interest"
#' occurs after transient initial bias in performance (e.g., initial task learning needs to
#' occur before the target behavior can be effectively measured). The following functions use a
#' stepwise method to first estimate the rate of change in the outcome variable, then use these rates to
#' transform the variable of time into a saturating interpolation between 1 [starting offset] and 0
#' [asymptotic level], and finally include the time variable in the corresponding
#' [\code{g}]\code{lm}[\code{er}] model:
#'
#' \itemize{
#' \item \code{\link{TElm}} wraps this method for \code{\link[stats]{lm}}
#' \item \code{\link{TEglm}} wraps this method for \code{\link[stats]{glm}}
#' \item \code{\link{TElmem}} wraps this method for \code{\link[lme4]{lmer}}
#' \item \code{\link{TEglmem}} wraps this method for \code{\link[lme4]{glmer}}
#' }
#'
#'
#'
#' @docType package
#' @name TEfits
#' @aliases TEfits
NULL
