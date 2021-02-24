
#' Fit a time-evolving model with Stan using brms
#' 
#' Formats and runs a brms model for a time-evolving nonlinear function. Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' @param formIn Formula to fit. See examples.
#' @param dataIn Data frame, from which to fit the model.
#' @param iter Number of iterations to run the model.
#' @param chains Number of chains to run the model.
#' @param priorIn Optional argument to pass priors to the \code{brms} model, which augment the TEfit-default rate prior.
#' @param ... Further arguments passed to the brms model
#' @param tef_control_list A list of control parameters passed in by \code{tef_control()}
#'
#' @return
#' @export
#'
#' @examples
#' 
#' ## Default model formula is exponential change, with no covariates or random effects
#' m <- TEbrm(
#' acc ~ trialNum
#' ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
#' ,dataIn = anstrain_s1
#' )
#' 
#' summary(m)
#' conditional_effects(m)
#' 
#' \dontrun{
#' 
#' ## using the \code{tef_change_expo3} function to construct the model formula, with random effects
#' m <- TEbrm(
#' acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
#' ,dataIn = anstrain
#' ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
#' )
#' 
#' }
TEbrm <- function(
  formIn
  ,dataIn
  ,iter = 1000
  ,chains = 3
  ,priorIn = c()
  , ... 
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
  
  ## ## Put it together into a formula
  bForm <- brmsformula(paste(
    attr(rhs_form,'lhs')
    ,'~'
    , rhs_form
  )
  ,nl=T)
  
  for(curPar in names(attr(rhs,'parForm'))){
    bForm <- bForm + lf(formula = paste(
      curPar
      ,paste(attr(rhs,'parForm')[[curPar]],collapse='')
    ))
  }
  
  
  
  
  bPrior <- set_prior(paste0('normal(',round(log(midTime,base=tef_control_list$expBase),3),','
                             ,round(log(midTime,base=tef_control_list$expBase)/2,3),')')
                      ,nlpar = names(attr(rhs,'parForm'))[grep('rate',tolower(names(attr(rhs,'parForm'))))][1]
  )
  
  if(length(priorIn) > 0){bPrior <- bPrior + priorIn}
  
  
  modOut <- brm(bForm
                ,data = attr(rhs_form,'data')
                ,iter = iter
                ,prior = bPrior
                ,chains=chains
                ,...
  )
  
  return(modOut)

  if(F){
    formIn <- acc ~ tef_change_expo3('trialNum')
    
    m <- TEbrm(
           acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
             ,dataIn = anstrain
             ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
           )
    
    m <- TEbrm(
      acc ~ trialNum
      ,dataIn = anstrain_s1
      ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')
    )
    
  }
  }
