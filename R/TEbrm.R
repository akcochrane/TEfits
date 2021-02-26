
#' Fit a time-evolving model with Stan using brms
#'
#' Formats and runs a \code{\link[brms]{brm}s} model for a time-evolving nonlinear function. Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' When specifying statistical families, it is \emph{extremely highly recommended} to specify an "identity" link function,
#' and then [if appropriate] specifying a link function using the \code{link_start_asym} argument. See example.
#'
#' Currently supported model constructor functions  are:
#' \itemize{
#' \item{\code{tef_change_expo3} -- 3-parameter exponential (start, [inverse] rate, and asymptote) -- rate is log of time to some proportion remaining, default is log2 of time to 50 percent remaining}
#' }
#'
#' @note
#' Default priors and parameter boundaries are implemented, but all users would benefit from
#' re-fitting models with different priors in order to ensure that inferences are not biased
#' by defaults. The assumptions that guided the creation of the default priors may not be
#' appropriate for your data. If no \code{link_start_asym} function is used (i.e., the default
#' 'identity') then start and asymptote priors are Gaussian with the response variable's mean, and double
#' the response variable's SD. If a \code{link_start_asym} function is used (e.g., 'log' or
#' 'inv_logit') then start and asymptote priors are Gaussian with a mean of zero and a SD of 3 (which
#' may place quite a bit of the prior density at values more extreme than appropriate for many
#' users' data). Default [log time constant] rate parameter's prior is Gaussian centered at the
#' log of the mean of the time variable (with the base of the log defined in \code{tef_control_list}).
#' The SD of this prior is 1/3 of the mean, and boundaries are implemented at extreme values. All
#' other priors are \code{\link[brms]{brm}} defaults.
#' Use \code{brms::prior_summary} to examine priors from a fitted model object.
#'
#' @param formIn A formula, with the time-varying response variable on the left, followed by \code{~}.  The right side must be either [A] a single variable corresponding to the dimension of time, or [B] a call to a \code{TEfits} constructor function such as \code{\link{tef_change_expo3}}. See examples.
#' @param dataIn Data frame, from which to fit the model.
#' @param ... Further arguments passed to the brms model
#' @param iter Number of iterations to run the model.
#' @param chains Number of chains to run the model.
#' @param priorIn Optional argument to pass priors to the \code{brms} model, alongside the TEfit-default rate prior. If you provide any, you will likely need to provide priors for all nonlinear parameters. \code{brm} error messages tend to be very helpful in this regard.
#' @param algorithm The algorithm to use when fitting the \code{\link[brms]{brm}} model
#' @param link_start_asym Link function to use for the start and asymptote parameters. Defaults to what is passed from formIn. Otherwise, most likely to be 'log' or 'inv_logit'
#' @param tef_control_list A list of control parameters passed in by \code{tef_control()}
#'
#' @seealso
#' For additional flexibility, and full explanations of model options, see \code{\link[brms]{brms-package}}.
#'
#' For other approaches to time-evolving models, see \code{\link{TEfits}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## Default model formula is exponential change, with no covariates or random effects
#' m1 <- TEbrm(
#' acc ~ trialNum # equivalent to `acc ~ tef_change_expo3('trialNum')`
#' ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
#' ,dataIn = anstrain_s1
#' )
#'
#' prior_summary(m1)
#' summary(m1)
#' conditional_effects(m1)
#'
#' ## using the tef_change_expo3 function to construct the model formula, with random effects
#' m2 <- TEbrm(
#' acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
#' ,dataIn = anstrain
#' ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
#' )
#'
#' ## Estimate accuracy using a more appropriate [bernoulli] response function,
#' ## ## and also estimate the start and asymptote parameters using invert-logit links
#' m3 <- TEbrm(
#' acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
#' ,dataIn = anstrain
#' ,link_start_asym = 'inv_logit'
#' ,family=bernoulli(link='identity')
#' )
#'
#' ## Fit a time-evolving logistic mixed-effects model (see, e.g., Cochrane et al., 2019, AP&P, 10.3758/s13414-018-01636-w)
#' m4 <- TEbrm(
#' resp ~ tef_link_logistic(
#' tef_change_expo3('trialNum', parForm = ~ (1|subID))
#' , linkX = 'ratio' )
#' ,family=bernoulli(link='identity')
#' ,iter = 4000
#' ,dataIn = anstrain
#' )
#'
#' }
TEbrm <- function(
  formIn
  ,dataIn
  , ...
  ,iter = 1000
  ,chains = 3
  ,priorIn = c()
  ,algorithm = "sampling"
  ,link_start_asym = ''
  ,tef_control_list=TEfits::tef_control()
){

  require(brms)

  ## ## Get RHS of formula
  {
    tryCatch({
      if(class(formIn[[3]])=='name'){ # the bivariate case [default & simple]
        rhs <- tef_change_expo3(as.character(formIn[[3]]))
      }else{
        rhs <- eval(formIn[[3]])
      }
      minTime <- min(dataIn[,attr(rhs,'timeVar')],na.rm=T)
      maxTime <- max(dataIn[,attr(rhs,'timeVar')],na.rm=T)
      midTime <- mean(c(minTime,maxTime))
      rhs_form <- gsub('TIMEVAR_MINIMUM',minTime,attr(rhs,'formula'))
    },error = function(error){stop('\nInput formula is not formatted properly')})

    ## Add the rest of the formula (dataIn and LHS)
    attr(rhs_form,'lhs')  <- as.character(formIn[[2]])
    attr(rhs_form,'data') <- dataIn ; rm(dataIn)
  }

  # if(link_start_asym == ''){
  #   attr(rhs,'link_start_asym') <- 'identity' ##ISSUE## need to add this to the various constructor functions
  # }
link_start_asym <- attr(rhs,'link_start_asym')

  ##ISSUE##  THIS WILL BREAK IF RATE ISN'T EXACTLY IDENTIFIED BY ONE PARAMETER, so need to have the changefun constructor ID the "main" names for the rate, asym, and start (the things that should have priors)
  ##ISSUE## THERE'S ALSO NO GUARANTEE THIS IS GOOD FOR NON-EXPO3
  ##ISSUE## Need to make this play nicely with the tef_control_list
  ##ISSUE## make sure that adding another prior overwrites it, and doesn't break it

  bPrior <- set_prior(paste0('normal(',round(log(midTime,base=tef_control_list$expBase),3),',' ##ISSUE##  The base is already (and should be) defined in the constructor
                             ,round(log(midTime,base=tef_control_list$expBase)/3,3),')')
                      ,nlpar = names(attr(rhs,'parForm'))[grep('rate',tolower(names(attr(rhs,'parForm'))))][1]
                      ,ub = round(log((maxTime-minTime)*2,base=tef_control_list$expBase),3)
                      ,lb = round(log(
                        ((maxTime-minTime)/nrow(attr(rhs_form,'data')))*2
                        ,base=tef_control_list$expBase),3)

  )

  ## ## Put it together into a formula ((THINK ABOUT SPLITTING THIS INTO PF AND CHANGE))
  bForm <- brmsformula(paste(
    attr(rhs_form,'lhs')
    ,'~'
    , rhs_form
  )
  ,nl=T)

  if(link_start_asym == 'identity'){transformed <- ''}else{transformed <- 'Xform'}

  for(curPar in names(attr(rhs,'parForm'))){
    # define the parameter formula
    bForm <- bForm + lf(formula = paste0(
      curPar
      ,paste(attr(rhs,'parForm')[[curPar]],collapse='')
    ))
    # if there is a link function, overwrite the start and asymptote parameter formulas
    suppressMessages({
      if( any(grep('start',tolower(curPar))) || any(grep('asym',tolower(curPar)) ) ){
        if(transformed == 'Xform'){
          bForm <- bForm + nlf(formula = paste0(
            curPar
            ,' ~ '
            ,link_start_asym,'('
            ,curPar,transformed
            ,')'
          ))
          bForm <- bForm + lf(formula = paste0(
            curPar,transformed
            ,paste(attr(rhs,'parForm')[[curPar]],collapse='')
          ))
          if(length(priorIn) == 0){
          bPrior <- bPrior + set_prior('normal(0,3)', nlpar = paste0(curPar,transformed))}
        }else{
          if(length(priorIn) == 0){
            bPrior <- bPrior + set_prior(paste0('normal('
                                                ,signif(mean(attr(rhs_form,'data')[,attr(rhs_form,'lhs')],na.rm=T),4)
                                                ,','
                                                ,signif(sd(attr(rhs_form,'data')[,attr(rhs_form,'lhs')],na.rm=T)*2,4)
                                                ,')')
                                         , nlpar = paste0(curPar,transformed))
          }
        }

      }
    })
  }

  if(!is.null(attr(rhs,'constantPar_prior'))){
    bPrior <- bPrior + attr(rhs,'constantPar_prior')
  }

  if(length(priorIn) > 0){bPrior <- bPrior + priorIn}

  if(algorithm == 'sampling'){
    modOut <- brm(bForm
                  ,data = attr(rhs_form,'data')
                  ,iter = iter
                  ,prior = bPrior
                  ,chains=chains
                  ,...
    )
  }else{
    nFails = 0 ; success = F ; while(!success && nFails < 5){
      modOut <- brm(bForm
                    ,data = attr(rhs_form,'data')
                    ,iter = iter
                    ,prior = bPrior
                    ,algorithm = algorithm
                    ,...
      )
      {. <- posterior_summary(modOut)
        success <- T}
    }}

  return(modOut)

  if(F){ # for testing

    library(TEfits)
    formIn <- acc ~ tef_change_expo3('trialNum')

    m1 <- TEbrm(
      acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
      ,dataIn = anstrain
      ,link_start_asym = 'inv_logit'
      ,family=bernoulli(link='identity')
    )

    m2 <- TEbrm(
      acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
      ,dataIn = anstrain
      ,algorithm = 'fullrank'
    )

    m3 <- TEbrm(
      acc ~ trialNum # equivalent to `acc ~ tef_change_expo3('trialNum')`
      ,dataIn = anstrain_s1
      ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
    )

    m4 <- TEbrm(
      acc ~ tef_change_expo3('trialNum')
      ,dataIn = anstrain_s1
      ,link_start_asym = 'inv_logit'
    )

    source('c:/users/ac/google drive/functions/fitPack/tef_link_logistic.R')
    formIn <- resp~ tef_link_logistic( tef_change_expo3('trialNum') , linkX = 'ratio' )
    formIn_r <- eval(formIn[[3]])

    m5 <- TEbrm(
      resp ~ tef_link_logistic( tef_change_expo3('trialNum') , linkX = 'ratio' )
      ,dataIn = anstrain_s1
    )


  }

}
