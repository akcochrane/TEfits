#' Run a brm model with ADVI
#'
#' Uses Stan's stochastic gradient ascent methods "fullrank" or "meanfield" rather than full
#' Bayesian sampling. Is likely to be much faster than typical sampling, but somewhat
#' less accurate. This function fits the model 10 times and returns the output
#' with the highest Bayesian R-squared. \strong{Beware: This
#' has a tendency to crash R sometimes.}
#'
#' Stochastic gradient ascent in Stan uses Automatic Differentiation Variational Inference (ADVI).
#'
#' @note
#' Re-fitting a model 10 times is not comprehensive. If using ADVI, it is recommended
#' to use \code{TEbrm_advi} multiple times, and choose the best using a comparison such as
#' \code{\link[brms]{add_criterion}(model,'loo')}.
#'
#' @seealso
#'
#' \code{\link[rstan]{vb}} and \link{http://mc-stan.org}
#'
#' @param formIn Model formula, as in \code{\link[brms]{brm}}.
#' @param dataIn Data, as in \code{\link[brms]{brm}}.
#' @param ...   Any other argument to pass to \code{\link[brms]{brm}}.
#' @param algorithm which ADVI algorithm to use: "meanfield" or "fullrank".
#' @param quiet Progress is printed by default, but can be suppressed with quiet=T.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## can be used in place of brm
#' m <- TEbrm_advi(ratio ~ resp, anstrain_s1)
#' summary(m)
#' conditional_effects(m)
#'
#' ## Use in the context of TEfits
#' m1 <- TEbrm(
#' acc ~ tef_change_expo3('trialNum')
#' ,dataIn = anstrain_s1,
#' ,algorithm = 'meanfield'
#' )
#'
#' }
TEbrm_advi <- function(formIn,
                       dataIn
                       , ...
                       ,algorithm = 'fullrank',
                       quiet = F){
  suppressMessages({require(brms)})
  m_fr <- list() ; m_fr_r2 <- c()  ; tryNum = 0 ; tmpMod <- NULL
  if(!quiet){ cat('Setting up the model...\n') }
  while(length(m_fr) < 10 && tryNum < 100){
    try({suppressMessages({suppressWarnings({

      if(is.null(tmpMod)){
        tmpMod <- brm(formIn,
                      data = dataIn
                      , iter = 20000
                      , ...
                      ,grad_samples = 3
                      ,init_r = .02
                      , output_samples = 2000
                      , silent = T
                      ,refresh = 0
                      ,algorithm = algorithm)
        if(!quiet){ cat('Fitting the model...\n[') }
      }else{
        tmpMod <- update(tmpMod
                         ,grad_samples = 3
                         , output_samples = 2000
                         ,silent = T
                         ,refresh = 0
        )
      }

      tmpMod <- add_criterion(tmpMod, 'bayes_R2')
      m_fr[[length(m_fr) + 1]] <- tmpMod
      m_fr_r2[length(m_fr_r2) + 1] <- mean(tmpMod$criteria$bayes_R2)

    })})
      Sys.sleep(.5)
      if(!quiet){ cat(length(m_fr)*10,'% ',sep='') }
    },silent=F)
    tryNum <- tryNum + 1
  } ;if(!quiet){ cat(']')}

  if(length(m_fr) < 1){stop('Something went wrong. Please check your data and model specification.')}

  outMod <- m_fr[[which.max(m_fr_r2)]]

  return(outMod)
}
