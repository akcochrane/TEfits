#' Run a brm model with ADVI
#'
#' Uses Stan's stochastic gradient ascent methods "fullrank" or "meanfield" rather than full
#' Bayesian sampling. Is likely to be faster than typical sampling, but possibly
#' less accurate. This function fits the model several times and returns the output
#' with the highest Bayesian R-squared. \strong{Beware: This
#' has a tendency to crash R sometimes.}
#'
#' Stochastic gradient ascent in Stan uses Automatic Differentiation Variational Inference (ADVI).
#'
#' @note
#' Re-fitting a model within this function is not comprehensive. If using ADVI, it is recommended
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
                       dataIn = data.frame()
                       , ...
                       ,algorithm = 'fullrank'
                       ,conv_thresh = .2
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
  if(!quiet){ cat('Setting up the model...\n') }

  while( (max_mod_diff_d > conv_thresh) && tryNum < 100){
    m_fr <- list() ; m_fr_r2 <- c()  ;
    while(length(m_fr) < 4 && tryNum < 100){
      try({suppressMessages({suppressWarnings({

        if(is.null(tmpMod)){
          tmpMod <- brm(formIn,
                        data = dataIn
                        , iter = 10000
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

  if(!quiet){ cat(']')}

  if(max_mod_diff_d == 1E3){stop('Something went wrong. Please check your data and model specification.')}

  mod1 <- modList[[compDF$mod1]]
  mod2 <- modList[[compDF$mod2]]

  mod1$max_mod_diff_d <- mod1$max_mod_diff_d <- max_mod_diff_d

  if(mean(mod1$criteria$bayes_R2) > mean(mod2$criteria$bayes_R2)){
    return(mod1)
  }else{
    return(mod2)}
}
