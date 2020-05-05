
#' Refit a TEfitAll model with brms
#'
#' Passes a \code{\link{TEfitAll}} model to [nonlinear mixed-effects Bayesian] fitting using the
#' \code{\link[brms]{brms}} package. Note that, due to the extensive time needed to
#' fit \code{\link[brms]{brms}} models,
#' this function is less tested than most functions in the \code{TEfits} package.
#'
#' \code{TEfitAll} \code{bernoulli} models are fit using either \code{bernoulli} or \code{Beta} response
#' distributions in \code{brms} depending on whether the \code{TEfitAll} distribution is
#' binary. \code{TEfitAll} \code{logcosh} models are fit using a \code{asym_laplace} response distribution
#' in brms.
#'
#' @param TEs3s TEfitAll model
#' @param nIter number of iterations
#' @param nChains number of chains
#' @param nCores number of cores
#' @param errFun the error function to use. Defaults to the same as the TEfitAll model, if possible.
#' @param prior_dispersion This number, multiplied by the SD of each TEfitAll parameter, is used as the prior SD for that parameter.
#'
#' @note
#' Under development. Partial functionality.
#'
#' @return A \code{\link[brms]{brms}} nonlinear mixed-effects model object.
#'
#' @export
tef_fitAll2brms <- function(TEs3s,nIter= 2000,nChains=3,nCores=2,errFun=NA,prior_dispersion=2){


  require(brms)
  # par_lims <- TEs3s$allFitList[[1]]$modList$parLims
  pars_orig <- TEs3s$allFitList[[1]]$modList$pNames
  pars <- gsub('_','',pars_orig)

  groupingVarName <- attr(TEs3s$fitSummary,'grouping_var')

  varIn <- data.frame(); for(curGroup in 1:length(TEs3s$allFitList)){
    subDat <- data.frame(TEs3s$allFitList[[curGroup]]$data)
    subDat[,groupingVarName] <-
      rownames(TEs3s$allFits)[curGroup]
    varIn <- rbind(varIn,subDat)}

    brmForm <- brmsformula(as.formula(paste(
      TEs3s$allFitList[[1]]$modList$respVar,'~',
      gsub('_','',as.character(TEs3s$allFitList[[1]]$modList$modl_fun)[[3]])
    ))
    ,as.formula(paste(paste(pars,collapse='+'),'~(1||',groupingVarName,')'))
    ,nl=T)

  ## make priors (better to have normal guided by TEfit result and bounded by par_lims)
  se2sd <- sqrt(length(TEs3s$allFitList))
  brmPriors <- set_prior(paste0('normal(',TEs3s$fitSummary['mean',pars_orig[1]],',',
                                TEs3s$fitSummary['stdErr',pars_orig[1]]*se2sd*prior_dispersion,')'),
                         nlpar=pars[1] ## ,ub=par_lims$parMax[1],lb=par_lims$parMin[1]
  ) ; if(length(pars)>1){
    for(curPar in 2:length(pars)){
      brmPriors <- brmPriors+
        set_prior(paste0('normal(',TEs3s$fitSummary['mean',pars_orig[curPar]],',',
                         TEs3s$fitSummary['stdErr',pars_orig[curPar]]*se2sd*prior_dispersion,')'),
                  nlpar=pars[curPar] ## ,ub=par_lims$parMax[curPar],lb=par_lims$parMin[curPar]
        )
    }}

  ## NEED TO ALSO HAVE VARIANCE PRIORS; HERE AND EVERYWHERE, HAVE VARIANCE PRIORS BE LOGNORMAL, WITH
  ## -3 ALWAYS BEING AT -4 SD AND +4SD BE AN UNLIKELY LARGE NUMBER

  ## WHEN IN DOUBT, DEFAULT TO LOGNORMAL(1,1)

  if(is.na(errFun)){errFun <- as.character(unique(TEs3s$fitSummary$errFun))}

  # Transform errFun into link functions
  errorFun <- switch(errFun,
                     'rmse' = gaussian(),
                     'exGauss_mu' = exgaussian(),
                     'ols' = gaussian(),
                     'bernoulli' = bernoulli(link='identity'),
                     'logcosh' = asym_laplace()
  )

  if(errFun =='bernoulli'){
    if(min(varIn[,1],na.rm=T)<0 || max(varIn[,1],na.rm=T) > 1){
      cat('The response variable is outside [0,1].')}
    else{
      if(!all(unique(na.omit(varIn[,1]))[1:2]==c(0,1))){
        cat('edge correction [.0001] was applied and a beta response distribution was used.')
        varIn[,1] <- (varIn[,1]*.9998)+.0001

        errorFun <- Beta(link = "identity")
      }
    }
  }

  ## fit model
  brmModel <- brm(brmForm,
                  varIn,
                  prior = brmPriors,
                  chains = nChains,
                  family = errorFun,
                  iter = nIter,
                  thin=max(c(1,floor(nIter/4000))),
                  cores = getOption("mc.cores",nCores),
                  control = list(adapt_delta = .95,
                                 max_treedepth = 50))

  return(brmModel)
}
