
#' Refit a TEfitAll model with brms
#'
#' Passes a \code{\link{TEfitAll}} model to [nonlinear mixed-effects Bayesian] fitting using
#' \code{\link[brms]{brms-package}}. Note that, due to the extensive time needed to
#' fit \code{\link[brms]{brms-package}} models,
#' this function is less tested than most functions in the \code{TEfits} package. Functionality is
#' \strong{experimental}.
#'
#' Priors for nonlinear parameters are informed by the distributions of parameters in the \code{TEfitAll} object [models].
#' However, any fixed effects should be minimally influenced by these priors
#'
#' \code{TEfitAll} \code{bernoulli} models are fit using either \code{bernoulli} or \code{Beta} response
#' distributions in \code{brms} depending on whether the \code{TEfitAll} distrIibution is
#' binary. \code{TEfitAll} \code{logcosh} models are fit using a \code{asym_laplace} response distribution
#' in brms predicting the .5 quantile.
#'
#' @param TEs3s TEfitAll model
#' @param fixef Parameters vary as random effects by the TEs3s grouping variable. However, if you have main effects (e.g., group differences), enter them \emph{as a data frame} here.
#' @param nIter number of iterations
#' @param nChains number of chains
#' @param nCores number of cores
#' @param errFun the error function to use. Defaults to the same as the TEfitAll model, if possible.
#' @param prior_dispersion This number, multiplied by the SD of each TEfitAll parameter, is used as the prior SD for that parameter.
#'
#' @note
#' Under development. Partial functionality.
#'
#' @return A \code{\link[brms]{brms-package}} nonlinear mixed-effects model object.
#'
#' @examples
#' \dontrun{
#' dat <- anstrain
#' dat$condition <- rep(c('A','B'),each=500)
#'
#' # Model with time and one categorical fixed effect
#' mod_tef <- TEfitAll(dat[,c('acc','trialNum')], groupingVar = dat$subID)
#' mod_brm <- tef_fitAll2brms(mod_tef,nChains=1,fixef=data.frame(condition=dat$condition))
#'
#' # Model with time, one categorical fixed effect, and one by-groupingVar (subID) random slope
#' dat$absRat <- scale(abs(dat$ratio))
#' mod_tef <- TEfitAll(dat[,c('acc','trialNum',"absRat")], groupingVar = dat$subID,covarTerms=list(pRate=c(F)))
#' mod_brm <- tef_fitAll2brms(mod_tef,nChains=1,fixef=data.frame(condition=dat$condition))
#' }
#'
#' @export
tef_fitAll2brms <- function(TEs3s,fixef=NA,nIter= 2000,nChains=3,nCores=2,errFun=NA,prior_dispersion=2){

  ## To do:
  ##
  ## ## have a fixed=c() argument, to wedge between 'pAsym ~' and '(1||groupingVar)' and similar constructions
  ##
  ## ## fit in a loop that keeps trying until a fit with samples, so that the user won't get something out with no samples
  ##
  ## ## probably should improve the docs, expecially around cores and chains. Someone who's never used brms should be able to get the broad strokes here.
  ## ## Also, seriously think about defaulting to one core and one chain. Given the likelihood of pathological sampling (the need for looping to sample), it's better.

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

    ## Need to look at a TEs3 object and see whether the full data frame is there, or only the relevant variables.
  ## ## if only the relevant variables, then need to get fixefs from the names of an input data frame, and
  ## ## merge the data frames from the TEs3 model and the new data.
  if(!is.data.frame(fixef)){fixefNames <- '1'}else{
    fixefNames <- names(fixef)
    varIn <- cbind(varIn,fixef)
  }


  fixefs <- paste(paste0(fixefNames,'+'),collapse='')
  parForm <- as.formula(paste(paste(pars,collapse='+'),'~',fixefs,'(1||',groupingVarName,')'))

    brmForm <- brmsformula(as.formula(paste(
      TEs3s$allFitList[[1]]$modList$respVar,'~',
      gsub('_','',as.character(TEs3s$allFitList[[1]]$modList$modl_fun)[[3]])
    ))
    ,parForm
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
