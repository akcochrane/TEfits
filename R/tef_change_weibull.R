#' Construct a 4-parameter Weibull function of change
#'
#' By defining the model variable associated with time (e.g., trial number), and
#' formulas defining each of the nonlinear parameters of time-related change,
#' this function constructs a model that can then be passed to functions for fitting
#' the model (e.g., \code{\link{TEbrm}}).
#'
#' Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' @param timeVar String. The name of the variable in the model that corresponds to time. The variable of time should be positive and numeric, and the function of change should be expected to happen with increasing time.
#' @param parForm The right-hand side of the formula defining all nonlinear parameters as well as the null [non-time-varying] model.
#' @param startForm The right-hand side of the formula defining the start parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter.
#' @param rateForm The right-hand side of the formula defining the rate parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter.
#' @param asymForm The right-hand side of the formula defining the asymptote parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter.
#' @param shapeForm The right-hand side of the formula defining the weibull shape (i.e., acceleration or deceleration) parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter. Shape is estimated on a log scale. Following this, a shape parameter equal to 0 makes the weibull function equivalent to the 3-parameter exponential function.
#' @param changeBase Scalar. The base of the log (e.g., 2 or \code{exp(1)}) of the change function.
#' @param rateBase  Scalar. The base of the log (e.g., 2 or \code{exp(1)}) of the rate [time constant].
#'
#' @seealso
#' \code{\link{TEbrm}} for examples of how to use this function in specifying models.
#'
#' @export
#'
#' @examples
#' equation_to_fit <- tef_change_weibull('timeVar',parForm = ~ xvar1*xvar2) # both variables should be numeric for TEfit methods! TEbrm should work with factors as well
#'
#' equation_to_fit <- tef_change_weibull('timeVar'
#' , parForm = ~ xvar1   # overall parameter formula; overwritten for time-evolving formulas below, leaving this as just the null model
#' , startForm = ~ xvar2 # start parameter's regression model
#' , rateForm = ~ (1|participantID)  # rate parameter's [mixed-effects] regression model
#' , asymForm = ~ xvar3 # asymptote parameter's regression model
#' , shapeForm = ~ (1|participantID)  # shape parameter's [mixed-effects] regression model
#' )
#'
tef_change_weibull <- function(timeVar
                             ,parForm = ~ 1
                             ,startForm= ~ 1
                             ,rateForm=  ~ 1
                             ,asymForm=  ~ 1
                             ,shapeForm= ~ 1
                             ,changeBase=2
                             ,rateBase=2
){
  if(is.character(timeVar) || is.factor(timeVar)){

    {
      parForms <- list()

      # # this is to have a single covar for them all (overridden below)
      for(whichPar in c('pStart','pRate','pAsym','pShape')){
        parForms[[whichPar]] <-  as.formula( tef_parseParFormula(parForm, label = whichPar) )
      }

      # # for specific covars
      if(as.character(startForm)[2] != '1' || is.numeric(startForm)){
        parForms[['pStart']] <- tef_parseParFormula(startForm, label = 'pStart')
      }
      if(as.character(rateForm)[2] != '1' || is.numeric(rateForm)){
        parForms[['pRate']] <- tef_parseParFormula(rateForm, label = 'pRate')
      }
      if(as.character(asymForm)[2] != '1' || is.numeric(asymForm)){
        parForms[['pAsym']] <- tef_parseParFormula(asymForm, label = 'pAsym')
      }
      if(as.character(shapeForm)[2] != '1' || is.numeric(shapeForm)){
        parForms[['pShape']] <- tef_parseParFormula(shapeForm, label = 'pShape')
      }
    }

    nullFun <- tef_parseParFormula(parForm, label = 'null')

    ## ISSUE ## just should have arguments inherited from the expo3, rather than typing everything out.


    # expBase defines time constant; invert expBase to find percent of change completed, e.g., 2^(-1)=.5 of change remaining
    # which means that, like exponential, pR is log(,rate=rateBase) of time to expBase^(-1) of change remaining
    # Then, pShape is the [scaled log(,base=expBase) of the] slope of the function at expBase^(-1)? more or less
    rhs <-  paste0(
      'pAsym+( (' ##ISSUE## look at parameters, make sure they're signed as expected. It seems as though these additions
      ,'pStart)-(' # # and subtractions are the inverse of those in tef_link_weibull(). Then this is reversed with a `1-` operator in tef_link_w, but this seems ugly
      ,'pAsym) )*'
      ,changeBase
      ,'^( -( ('
      ,timeVar
      ,'-TIMEVAR_MINIMUM'
      ,')/('
      ,rateBase
      ,'^(pRate'
      ,') ) )^( ',rateBase
      ,'^(pShape)) )'
    )

    rhsString <- rhs
    attr(rhsString,'parForm') <- parForms
    attr(rhsString,'formula') <- rhs
    attr(rhsString,'MEM') <- any(c(
      attr(parForms[['pStart']],'MEM')
      ,attr(parForms[['pRate']],'MEM')
      ,attr(parForms[['pAsym']],'MEM')
      ,attr(parForms[['pShape']],'MEM')
    ))
    attr(rhsString,'changeFun') <- 'weibull_4par'
    attr(rhsString,'timeVar') <- timeVar
    attr(rhsString,'nullFun') <- nullFun

    attr(rhsString,'changeBase') <- changeBase
    attr(rhsString,'rateBase') <- rateBase

    ## ## ISSUE ## ## make sure this isn't just overwritten by the linkfun
    try({
    attr(rhsString,'constantPar_prior') <-
      brms::prior(normal(0,3),nlpar = 'pShape')
    },silent=T)

    if(!attr(rhsString,'MEM')){
      rhsString <- gsub('pStart',attr(parForms[['pStart']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pRate',attr(parForms[['pRate']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pAsym',attr(parForms[['pAsym']], "equation"),rhsString,fixed = T)

      ## ## Here, include two vectors as attributes to this vector: first, logical of isIntercept, second, named of whichPar
      ## then can use these later to generate proposal values etc.
      ## should probably have genGuess be a function that is an attribute as well.
      attr(rhsString,'allPars') <- unlist(c(attr(parForms[['pStart']],"parameters"),
                                            attr(parForms[['pRate']],"parameters"),
                                            attr(parForms[['pAsym']],"parameters")))

      attr(rhsString,'whichPar') <- c(
        rep('pStart',length(attr(parForms[['pStart']],"parameters"))),
        rep('pRate',length(attr(parForms[['pRate']],"parameters"))),
        rep('pAsym',length(attr(parForms[['pAsym']],"parameters")))
      )

      attr(rhsString,'isIntercept') <- grepl('Intercept',attr(rhsString,'allPars'))

    }

    # # replace with fixed, if relevant ##ISSUE## Need to put this into Weibull
    for(curPar in 1:length(attr(rhsString, 'parForm'))){
      if(attr(attr(rhsString, 'parForm')[[curPar]],'is_fixed')){
        attr(rhsString,'formula') <- gsub(names(attr(rhsString, 'parForm'))[curPar]
                                          ,as.numeric(attr(rhsString, 'parForm')[[curPar]])
                                          ,attr(rhsString,'formula'),fixed = T)
      }
    }

    return(
      rhsString
    )
  }
}
