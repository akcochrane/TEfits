#' Run a brm model with ADVI
#'
#' Uses Stan's stochastic gradient ascent methods "fullrank" or "meanfield" rather than full
#' Bayesian sampling. Is likely to be faster than typical sampling for large models, but possibly
#' less accurate. This function fits the model several times and returns the best model
#' (initial model selection uses Bayesian R-squared, final model selection uses 10-fold
#' cross-validation). \strong{Beware: This re-fitting
#' has a tendency to crash R sometimes.}
#'
#' Stochastic gradient ascent in Stan uses Automatic Differentiation Variational Inference (ADVI).
#'
#' @note
#' Re-fitting a model within this function is not comprehensive. If using ADVI, it is recommended
#' to use \code{TEbrm_advi} multiple times, and choose the best using comparisons of fit, such as
#' \code{fit_model$criteria$kfold} (estimated using
#'  \code{\link[brms]{add_criterion}(fit_model,'kfold')}).
#'
#' @seealso
#'
#' \code{\link[rstan]{vb}} and \link{http://mc-stan.org}
#'
#' @param formIn Model formula, as in \code{\link[brms]{brm}}.
#' @param dataIn Data, as in \code{\link[brms]{brm}}.
#' @param ...   Any other argument to pass to \code{\link[brms]{brm}}.
#' @param algorithm Which ADVI algorithm to use: "meanfield" or "fullrank".
#' @param conv_thresh Re-fit models are compared, with the standardized distance (mean_diff / SD) being calculated. Models keep being re-fit until at least 2 models' largest standardized differences are smaller than this value (or until a certain number of models has been fit in total, which scales inversely with this value). The better-fitting of these two models is then returned. Values over 30 will cause an error, which should not be an issue for any normal use of this function.
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
                       dataIn = data.frame()
                       , ...
                       ,algorithm = 'fullrank'
                       ,conv_thresh = .5
                       ,quiet = F){

  compareMlist <- function(mlist){

    compDF <- data.frame()

    for(curMod1 in 1:length(mlist)){
      for(curMod2 in 2:length(mlist)){
        if(curMod2 > curMod1){

          curMaxD <- max(abs((fixef(mlist[[curMod1]])[,'Estimate'] - fixef(mlist[[curMod2]])[,'Estimate'])/
                               ((fixef(mlist[[curMod1]])[,'Est.Error'] + fixef(mlist[[curMod2]])[,'Est.Error'])/2)))

          compDF <- rbind(compDF,
                          data.frame(mod1 = curMod1
                                     ,mod2 = curMod2
                                     ,maxD = curMaxD))

        }
      }
    }
    return(compDF[which.min(compDF$maxD),])
  }

  suppressMessages({require(brms)})
  tryNum = 0 ;
  modList <- list()
  tmpMod <- NULL ; max_mod_diff_d <- 1E3
  maxTries <- round(20/conv_thresh)
  if(!quiet){ cat('Setting up the model...\n') }

  while( (max_mod_diff_d > conv_thresh) && tryNum < maxTries){
    m_fr <- list() ; m_fr_r2 <- c()  ;
    while(length(m_fr) < 2 && tryNum < maxTries){
      try({suppressMessages({suppressWarnings({

       # The rewriting of tmpMod is likely causing crashes; should be writing to a "fresh" object instead

        if(is.null(tmpMod)){
          tmpMod <- brm(formIn,
                        data = dataIn
                        , iter = 10000
                        , ...
                        ,grad_samples = 3
                        ,init_r = .02
                        , output_samples = 2000
                        ,elbo_samples = 500 # X samples to get ELBO
                        ,eval_elbo = 50 # Every X iterations
                        ,adapt_iter = 200 # X samples to get the stepsize
                        , silent = T
                        ,refresh = 0
                        ,algorithm = algorithm)
          if(!quiet){ cat('Fitting the model...\n[') }
        }else{
          tmpMod <- update(tmpMod
                           ,grad_samples = 3
                           , output_samples = 2000
                           ,elbo_samples = 500
                           ,eval_elbo = 50
                           ,adapt_iter = 200
                           ,silent = T
                           ,refresh = 0
          )
        }

        tmpMod <- add_criterion(tmpMod, 'bayes_R2')
        m_fr[[length(m_fr) + 1]] <- tmpMod
        m_fr_r2[length(m_fr_r2) + 1] <- mean(tmpMod$criteria$bayes_R2)

      })})
        Sys.sleep(.5)

      },silent=T)
      tryNum <- tryNum + 1
    }; if(!quiet){ cat('. ') }
    if(length(m_fr) < 1){stop('Something went wrong. Please check your data and model specification.')}

    modList[[length(modList) + 1]] <- m_fr[[which.max(m_fr_r2)]]

    if(length(modList)>1){
      compDF <- compareMlist(modList)

      max_mod_diff_d <- compDF$maxD

      # print(compDF)
    }

  }

  if(max_mod_diff_d == 1E3){stop('Something went wrong. Please check your data and model specification.')}

  suppressMessages({
  mod1 <- add_criterion( modList[[compDF$mod1]] , 'kfold', seed=T)
  mod2 <- add_criterion( modList[[compDF$mod2]] , 'kfold', seed=T)
  })

  if(!quiet){ cat(']')}

  mod1$crit_compare <- mod2$crit_compare <- loo_compare(mod1$criteria$kfold, mod2$criteria$kfold)

  mod1$max_mod_diff_d <- mod2$max_mod_diff_d <- max_mod_diff_d

  if(
    mod1$criteria$kfold$estimates['elpd_kfold','Estimate']
     >
    mod2$criteria$kfold$estimates['elpd_kfold','Estimate']
     ){
    return(mod1)
  }else{
    return(mod2)}
}
