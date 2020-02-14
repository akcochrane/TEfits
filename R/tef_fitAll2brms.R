
#' Refit a TEfitAll model with brms
#'
#' Passes a TEfitAll model to [hierarchical nonlinear Bayesian] fitting using the
#' brms package. Note that, due to the extensive time needed to fit brms models,
#' this function is even less tested than most functions in the TEfits package.
#'
#' TEfitAll 'bernoulli' models are fit using either 'bernoulli' or 'Beta' response
#' distributions in brms, depending on whether the TEfitAll distribution is
#' binary. TEfitAll 'logcosh' models are fit using a 'asym_laplace' response distribution
#' in brms.
#'
#' @param TEs3s TEfitAll model
#' @param nIter number of iterations
#' @param nChains number of chains
#' @param nCores number of cores
#' @param errFun the error function to use. Defaults to the same as the TEfitAll model, if possible.
#'
#' @export
#'
tef_fitAll2brms <- function(TEs3s,nIter= 2000,nChains=3,nCores=2,errFun=NA){

  # TO DO:
  # # make groupingVar actually have a name
  # # customization? (err, etc?)
  # # should have argument brmList=list(iter=2000,chains=0,fixefVars=NA)
  # # what returns?

  library(brms)
  # par_lims <- TEs3s$allFitList[[1]]$modList$parLims
  pars <- TEs3s$allFitList[[1]]$modList$pNames

  groupingVarName <- attr(TEs3s$fitSummary,'grouping_var')

  varIn <- data.frame(); for(curGroup in 1:length(TEs3s$allFitList)){
    subDat <- data.frame(TEs3s$allFitList[[curGroup]]$data)
    subDat[,groupingVarName] <-
      rownames(TEs3s$allFits)[curGroup]
    varIn <- rbind(varIn,subDat)}

  brmForm <- brmsformula(TEs3s$allFitList[[1]]$modList$modl_fun
                         ,as.formula(paste(paste(pars,collapse='+'),'~(1||',groupingVarName,')'))
                         ,nl=T)

  ## make priors (better to have normal guided by TEfit result and bounded by par_lims)
  se2sd <- sqrt(length(TEs3s$allFitList))
  brmPriors <- set_prior(paste0('normal(',TEs3s$fitSummary['mean',pars[1]],',',TEs3s$fitSummary['stdErr',pars[1]]*se2sd*2,')'),
                         nlpar=pars[1] ## ,ub=par_lims$parMax[1],lb=par_lims$parMin[1]
  ) ; if(length(pars)>1){
    for(curPar in 2:length(pars)){
      brmPriors <- brmPriors+
        set_prior(paste0('normal(',TEs3s$fitSummary['mean',pars[curPar]],',',TEs3s$fitSummary['stdErr',pars[curPar]]*se2sd*2,')'),
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
        varIn[,1] <- (x*.9998)+.0001

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
