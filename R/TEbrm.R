
#' Fit a time-evolving model with Stan using brms
#'
#' Formats and runs a \code{\link[brms]{brm}s} model for a time-evolving nonlinear regression.
#' This is the recommended way to fit models using \code{TEfits}.
#' Function is \strong{under development}
#' and is likely to change frequently.
#'
#' The variable of time should be positive and numeric, with the nonlinear model providing a regression as a
#' function of that time variable.
#'
#' The default number of iterations and chains is small, and intended largely for testing model specifications.
#' For final inferences from a model, it is highly recommended to run a model for many more iterations
#' (e.g., \code{iter=5000} or \code{iter=10000}).
#'
#' When specifying statistical families, it is \emph{extremely highly recommended} to specify an "identity" link function,
#' and then [if appropriate] specifying a link function using the \code{link_start_asym} argument. See example.
#'
#' Rates [time constants] are estimated on log scales. Within the exponent of the log estimation, the rates have an additive
#' offset corresponding to the median of the time variable; this allows the rate parameter priors and estimation to be zero-centered.
#'
#' Currently supported model constructor functions are:
#' \itemize{
#' \item{\code{\link{tef_change_expo3}} -- 3-parameter exponential (start, [inverse] rate, and asymptote) -- rate is log of time to some proportion remaining, default is \code{log2} of time to 50 percent remaining}
#' \item{\code{\link{tef_change_weibull}} -- 4-parameter weibull (start, [inverse] rate, asymptote, and shape) -- Augmented exponential function. Rate is log of time to some proportion remaining, default is \code{log2} of time to 50 percent remaining. Shape indicates acceleration (>0) or deceleration (<0) of the hazard function (which is constant in an exponential function).}
#' \item{\code{\link{tef_change_power3}} -- 3-parameter power (start, [inverse] rate, and asymptote) -- rate is log of time to some proportion remaining, default is \code{log2} of time to 25 percent remaining, in order to have parameter ranges be similar to the 3-parameter exponential.}
#' \item{\code{\link{tef_change_power4}} -- 4-parameter power (start, [inverse] rate, asymptote, and "previous learning time") -- Augmented power function, with a "amount of previous learning time" parameter that is estimated on the same scale as \code{rate}. Rate is log of time to some proportion remaining, default is \code{log2} of time to 25 percent remaining, in order to have parameter ranges be similar to the 3-parameter exponential.}
#' }
#'
#' Currently supported link functions are:
#' \itemize{
#' \item{\code{\link{tef_link_logistic}} -- logistic psychometric function, parameterized in terms of threshold and bias. Takes the output of a \code{tef_change_} function and augments it before passing to \code{TEbrm}}
#' \item{\code{\link{tef_link_weibull}} -- weibull (AKA Quick) psychometric function, parameterized in terms of threshold and shape. Takes the output of a \code{tef_change_} function and augments it before passing to \code{TEbrm}}
#' }
#'
#' @note
#' Default priors and parameter boundaries are implemented, but all users would benefit from
#' re-fitting models with various different priors in order to ensure that inferences are not biased
#' by defaults. The assumptions that guided the creation of the default priors may not be
#' appropriate for your data. If no \code{link_start_asym} function is used (i.e., the default
#' 'identity') then start and asymptote priors are Gaussian, with the response variable's mean and double
#' the response variable's SD. If a \code{link_start_asym} function is used (e.g., 'exp' or
#' 'inv_logit') then start and asymptote priors are Gaussian with a mean of zero and a SD of 3 (which
#' may place quite a bit of the prior density at values more extreme than appropriate for many
#' users' data). Default [log time constant] rate parameter's prior is Gaussian centered at the
#' log of the mean of the time variable (with the base of the log defined in \code{tef_control_list}).
#' The SD of this prior is 1/3 of the mean, and boundaries are implemented at extreme values. All
#' other priors are \code{\link[brms]{brm}} defaults.
#' Use \code{\link[brms]{prior_summary}} to examine priors from a fitted model object; see
#' \code{\link[brms]{set_prior}} for setting priors.
#'
#' It is \emph{highly recommended} that, if additional customization is desired (e.g., regarding priors),
#' to run \code{TEbrm} to create a model with a small number of iterations that is "close enough" to the
#' desired model specification, then use \code{\link[brms]{update.brmsfit}} to "fine-tune" your model directly (see example 10).
#'
#' @param formula A formula, with the time-varying response variable on the left, followed by \code{~}.  The right side must be either [A] a single variable name corresponding to the dimension of time, or [B] a call to a \code{TEfits} constructor function such as \code{\link{tef_change_expo3}}. See examples.
#' @param data Data frame, from which to fit the model.
#' @param ... Further arguments passed to the \code{\link[brms]{brm}s} model
#' @param chains Number of chains to run the model.
#' @param priorIn Optional argument to pass priors to the \code{brms} model, alongside the TEfit-default rate prior. If you provide any, you will likely need to provide priors for all nonlinear parameters. \code{brm} error messages tend to be very helpful in this regard. For more explicit and full control of priors, define all desired priors directly with the \code{prior} argument (which passes straight to \code{brm} and overwrites all other defined priors).
#' @param algorithm The algorithm to use when fitting the \code{\link[brms]{brm}} model. See \code{\link{TEbrm_advi}} for warnings and implementation of 'meanfield' or 'fullrank.'
#' @param link_start_asym Inverse of the link function to use for the start and asymptote parameters. Defaults to what is passed from formIn. Otherwise, the user would most likely to want to use 'exp' or 'inv_logit'. Refer to examples, and to the recommendation to use an "identity" link function rather than a statistical family's default link.
#' @param tef_control_list A list of control parameters passed in by \code{tef_control()}
#'
#' @seealso
#' For additional flexibility, and full explanations of model options, see \code{\link[brms]{brms-package}}. In
#' particular, the "..." argument that is passed to \code{\link[brms]{brm}} includes many important options, such
#' as increasing iterations (argument \code{iter})or parallelization (e.g., \code{cores = 3}).
#'
#' For other approaches to time-evolving models, see \code{\link{TEfits}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #-- #-- Example 01: Simple model
#' #> Default model formula is exponential change, with no covariates or random effects
#' m1 <- TEbrm(
#'   acc ~ trialNum   #> equivalent to `acc ~ tef_change_expo3('trialNum')`
#'   ,data = anstrain_s1
#' )
#'
#' prior_summary(m1)
#' summary(m1)
#' conditional_effects(m1)
#' hypothesis(m1,'pAsym_Intercept > pStart_Intercept') #> Test for learning, i.e., whether asymptote was reliably higher than start. With this limited data, that difference is not reliable
#'
#' #-- #-- Example 02: Random effects
#' #> using the tef_change_expo3 function to construct the model formula, with priors, and fixed and random effects
#' m2 <- TEbrm(
#'   acc ~ tef_change_expo3('trialNum',parForm = ~ sizeRat + (1|subID))
#'   ,data = anstrain
#'   ,priorIn = prior(normal(.5,.5),nlpar='pAsym') + prior(normal(.5,.5),nlpar='pStart')   #> for demonstration, also include non-default priors
#' )
#'
#' #-- #-- Example 03: Bernoulli family
#' #> Estimate accuracy using a more appropriate [bernoulli] response function,
#' #> > and also estimate the start and asymptote parameters using invert-logit links
#' m3 <- TEbrm(
#'   acc ~ tef_change_expo3('trialNum')
#'   ,data = anstrain_s1
#'   ,link_start_asym = 'inv_logit'
#'   ,family=bernoulli(link='identity')
#' )
#'
#' #-- #-- Example 04: Logistic PF
#' #> Fit a time-evolving logistic mixed-effects model (see, e.g., Cochrane et al., 2019, AP&P, 10.3758/s13414-018-01636-w).
#' #> > May take a few minutes to run.
#' m4 <- TEbrm(
#'   resp ~ tef_link_logistic(
#'      tef_change_expo3('trialNum', parForm = ~ (1|subID))
#'      ,linkX = 'ratio' )
#'   ,family=bernoulli(link='identity')
#'   ,iter = 4000   #> most models, in practice, will need more than the default number of iterations in order to converge as well as have a sufficient effective sample size (ESS)
#'   ,data = anstrain
#' )
#'
#' summary(m4) #> note the `exp` inverse link function on pStartXform and pAsymXform(i.e., log link for threshold values)
#' conditional_effects(m4, 'ratio:trialNum') #> The psychometric function steepens with learning; see ?brms::conditional_effects
#' cat(attr(m4$right_hand_side,'link_explanation')) #> An explanation of the link function is included
#'
#' #-- #-- Example 05: Weibull PF
#' #> Model change in a Weibull psychometric function's threshold
#' #> > (learning is change in the absolute stimulus strength at which accuracy is 75%)
#' d_tmp <- anstrain_s1   #> make temporary data
#' d_tmp$absRat <- abs(d_tmp$ratio)   #> calculate absolute stimulus strength
#' m5 <- TEbrm(
#'   acc ~ tef_link_weibull(
#'     tef_change_expo3('trialNum'),linkX = 'absRat')
#'   ,data = d_tmp
#' )
#'
#' #-- #-- Example 06: d prime
#' #> Model change in d-prime
#' d_tmp <- anstrain_s1 #> make temporary data
#' d_tmp$dprime <- tef_acc2dprime(d_tmp$acc, d_tmp$ratio > 0)   #> calculate by-trial d-prime; it isn't prototypical data for d-prime because `resp` doesn't categorize `acc` into "hits" and "false alarms", but it's a decent demonstration of the method.
#' m6 <- TEbrm(
#'   dprime ~  tef_change_expo3('trialNum')
#'   ,data = d_tmp
#' )
#'
#'#-- #-- Example 07: Power change
#' #> Rather than a 3-parameter exponential function of change, use a 3-parameter power function of change.
#' #> > Also, include a covariate for learning rate.
#' m7 <- TEbrm(
#'   acc ~ tef_change_power3('trialNum'
#'       ,rateForm = ~ sizeRat)
#'   ,data = anstrain_s1
#' )
#'
#' #-- #-- Example 08: Weibull change
#' #> Model learning as a Weibull function of time (3-parameter exponential with one additional "acceleration" or "deceleration" parameter, "pShape")
#' #> > May take a few minutes to run.
#' m8 <- TEbrm(
#'   acc ~ tef_change_weibull('trialNum')
#'   ,anstrain_s1
#' )
#'
#' #> Test whether learning is accelerating or decelerating, relative to a 3-parameter exponential
#' hypothesis(m8, 'pShape_Intercept = 0')    #> acceleration or deceleration is not evident in this task on this timescale
#'
#' #-- #-- Example 09: fixing a parameter
#' #> Fix a parameter to a constant rather than estimating it (here, fix asymptotic accuracy to 90 percent)
#' m9 <- TEbrm(
#'   acc ~ tef_change_expo3('trialNum'
#'                       ,asymForm = .9)
#'   ,data = anstrain_s1
#' )
#'
#' #-- #-- Example 10: updating a model
#' #> Fit a preliminary model with few iterations, adjust it to a different family, and update it with more iterations and chains
#' d_tmp <- anstrain_s1 #> make temporary data
#' d_tmp$acc_smooth <- tef_runningMean(d_tmp$acc)   #> get smoothed accuracy
#' m10_initial <-  TEbrm(
#'   acc_smooth ~ tef_change_expo3('trialNum')
#'   ,data = d_tmp
#'   ,iter = 200
#' )
#'
#' m10_final <- update(
#'  m10_initial
#'  ,family=student() #> heavier-tailed (t distribution) regression
#'  ,iter = 4000
#'  ,chains=4
#' )
#'
#' #-- #-- Example 11: Generalized Additive Models
#' #> Fit a generalized additive mixed-effects model with accuracy varying by time.
#' m11 <- TEbrm(acc ~ tef_change_gam('trialNum', groupingVar = 'subID')
#' ,data = anstrain)
#'
#' #-- #-- Example 12: Using a variational algorithm rather than full sampling
#' m12 <- TEbrm(acc ~ tef_change_expo3('trialNum')
#' ,algorithm = 'fullrank'
#' ,data = anstrain_s1)
#'
#' }
TEbrm <- function(
  formula
  ,data
  , ...
  ,chains = 3
  ,priorIn = c()
  ,algorithm = "sampling"
  ,link_start_asym = ''
  ,quiet=F
  ,tef_control_list=tef_control()
){
  formIn <- formula ; rm(formula)
  dataIn <- data ; rm(data)

  ##ISSUE## should match.call() or something, to get the original call and include it in the output (and TEfits version, etc.)

  ##ISSUE## re-work the NOTE to ensure conformity with the current prior method

  ##ISSUE## it seems that some versions of brms need priors for power4 prevTime, while others don't

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
      logMedTime <- round(
        log(
          median(dataIn[,attr(rhs,'timeVar')],na.rm=T)
          ,base=tef_control_list$rateBase)
        ,1)
      rhs_form <- gsub('TIMEVAR_MINIMUM',minTime,attr(rhs,'formula'))
      rhs_form <- gsub('TIMEVAR_LOG_MEDIAN'  ## changefuns to still change: weibull, power3, power4
                       ,logMedTime
                       ,rhs_form
      )
      rhs_form <- strsplit(rhs_form,', parameter_',fixed=T)
    },error = function(error){stop('\nInput formula is not formatted properly or data is missing.')})

    ## Add the rest of the formula (dataIn and LHS)
    attr(rhs_form,'lhs')  <- as.character(formIn[[2]])
    if(length(formIn[[2]]) == 3){ # for multi-part left-hand-sides
      attr(rhs_form,'lhs')  <-  paste0(formIn[[2]][2],formIn[[2]][1],formIn[[2]][3],collapse='')
    }


    ##ISSUE## it would be nice to have the brm model say the data "name" is the same as the input, rather than the attr(..etc). Would this take a match.call() or something?
    attr(rhs_form,'data') <- dataIn ; rm(dataIn)   }

  if(link_start_asym == ''){
    link_start_asym <- 'identity'##ISSUE## need to add this to the various constructor functions INSTEAD. these lines are a hack. The next lines are "real"
  }
  if(!is.null(attr(rhs,'link_start_asym'))){
    link_start_asym <- attr(rhs,'link_start_asym')
  }

  ##ISSUE## THIS WILL BREAK IF RATE ISN'T EXACTLY IDENTIFIED BY ONE PARAMETER, so need to have the changefun constructor ID the "main" names for the rate, asym, and start (the things that should have priors)... but as long as each changefun has exact matches to pStart, pAsym, and pRate, it should be OK right? does it work OK with fixed pars?
  ##ISSUE## THERE'S ALSO NO GUARANTEE THIS IS GOOD FOR NON-EXPO3
  ##ISSUE## Need to make this play nicely with the tef_control_list
  ##ISSUE## make sure that adding another prior overwrites it, and doesn't break it
  ##ISSUE## Split the prior definition into a different function for less ugliness and disorganization
  ##ISsuE## there's the classic problem of the nlpar intercept being non-zero-centered, while its covariates should have zero-centered priors. There are a couple routes forward... just having zero centered everything will bias results toward "instant" learning and nonsense starts... could add a median-time-var constant to the rate? That gets weirdly ad hoc, and then would require even more explanation. But might be the best.

  # cat(paste(
  #   attr(rhs_form,'lhs')
  #   ,'~'
  #   , rhs_form[[1]][1] # Need to ensure compatibility with thresholds!
  # ))

  if(attr(rhs,'changeFun') == 'GAM'){
    bForm <- brmsformula(paste(
      attr(rhs_form,'lhs')
      ,'~'
      , rhs_form[[1]][1] # Need to ensure compatibility with thresholds!
    )
    )


    if(length(priorIn) > 0){bPrior <- priorIn}else{bPrior <- NULL}


  }else{
    rate_prior_scale <- round(log(midTime,base=tef_control_list$rateBase)/3,3) ## use this to control it; refer to maxTime-minTime) instead probably
    bPrior <- set_prior(paste0('normal(0,'
                               ,rate_prior_scale,')')
                        ,nlpar = names(attr(rhs,'parForm'))[grep('rate',tolower(names(attr(rhs,'parForm'))))][1]
    )
    bPrior <- bPrior + set_prior(paste0('normal(',logMedTime,','
                                        ,rate_prior_scale,')')
                                 ,nlpar = names(attr(rhs,'parForm'))[grep('rate',tolower(names(attr(rhs,'parForm'))))][1]
                                 ,coef = 'Intercept'

                                 # ,ub = round(log((maxTime-minTime)*2,base=tef_control_list$rateBase),3) ##ISSUE## re-implement this, from the centered pars. Working around the "bounds can't be assigned with coefs" issue
                                 # ,lb = round(log(
                                 #   ((maxTime-minTime)/nrow(attr(rhs_form,'data')))*2
                                 #   ,base=tef_control_list$expBase),3)

    )

    ## ## Put it together into a formula ((THINK ABOUT SPLITTING THIS INTO PF AND CHANGE))
    ##ISSUE## Yes, need to split into PF and change



    bForm <- brmsformula(paste(
      attr(rhs_form,'lhs')
      ,'~'
      , rhs_form[[1]][1]
    )
    ,nl=T)
    if(length(rhs_form[[1]])>1){
      for(curComponent in 2:length(rhs_form[[1]])){
        bForm <- bForm + nlf(rhs_form[[1]][curComponent])
      }
    }

    if(link_start_asym == 'identity'){transformed <- ''}else{transformed <- 'Xform'}

    for(curPar in names(attr(rhs,'parForm'))){
      if(!is.numeric( attr(rhs,'parForm')[[curPar]] )){ # # don't do this stuff if the parameter was given as a constant
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
                bPrior <- bPrior + set_prior('normal(0,3)', nlpar = paste0(curPar,transformed), coef = 'Intercept')}
            }else{
              if(length(priorIn) == 0){
                # bPrior <- bPrior + set_prior(paste0('normal('
                #                                     ,0
                #                                     ,','
                #                                     ,signif(sd(attr(rhs_form,'data')[,attr(rhs_form,'lhs')],na.rm=T)*2,4)
                #                                     ,')')
                #                              , nlpar = paste0(curPar,transformed))

                bPrior <- bPrior + set_prior(paste0('normal('
                                                    ,signif(mean(attr(rhs_form,'data')[,attr(rhs_form,'lhs')],na.rm=T),4)
                                                    ,','
                                                    ,signif(sd(attr(rhs_form,'data')[,attr(rhs_form,'lhs')],na.rm=T)*2,4)
                                                    ,')')
                                             ,coef = 'Intercept'
                                             , nlpar = paste0(curPar,transformed))
              }
            }

          }
        })
      }
    }

    if(!is.null(attr(rhs,'constantPar_prior'))){
      bPrior <- bPrior + attr(rhs,'constantPar_prior')
    }
  }

  if(length(priorIn) > 0){bPrior <- bPrior + priorIn}
  # if(exists('prior')){bPrior <- prior} ##ISSUE## make sure this works under various conditions.

  if(algorithm == 'sampling'){
    modOut <- brm(bForm
                  ,data = attr(rhs_form,'data')
                  ,prior = bPrior
                  ,chains=chains
                  ,init_r = .02
                  ,...
    )
  }else{
    modOut <- TEbrm_advi(
      bForm
      ,dataIn = attr(rhs_form,'data')
      ,prior = bPrior
      ,algorithm = algorithm
      ,...
      ,quiet=quiet
    )
  }

  modOut$right_hand_side <- rhs

  return(modOut)

  if(F){ # for testing ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

    library(TEfits)
    formIn <- acc ~ tef_change_expo3('trialNum')

    m1 <- TEbrm(
      acc ~ tef_change_expo3('trialNum',parForm = ~ (1|subID))
      ,dataIn = anstrain
      ,link_start_asym = 'inv_logit'
      ,family=bernoulli(link='identity')
    )

  }

}
