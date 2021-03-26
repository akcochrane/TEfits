#' Construct a 4-parameter power function of change
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
#' @param startForm The right-hand side of the formula defining the start parameter. Overwrites \code{parForm} for this parameter.
#' @param rateForm The right-hand side of the formula defining the rate parameter. Overwrites \code{parForm} for this parameter.
#' @param asymForm The right-hand side of the formula defining the asymptote parameter. Overwrites \code{parForm} for this parameter.
#' @param prevTimeForm The right-hand side of the formula defining the log of the "previous time" parameter. Overwrites \code{parForm} for this parameter. The base of the log is \code{rateBase}.
#' @param rateBase  Number. The base of the log (e.g., 2 or \code{exp(1)}) of the rate [time constant].
#' @param propRemain Change rate is parameterized in terms of the \code{rateBase} log of time to this proportion of change remaining (i.e., \code{1-propRemain} of total change occurs in \code{rateBase^rateParameter} time.
#'
#' @seealso
#' \code{\link{TEbrm}} for examples of how to use this function in specifying models.
#'
#' @export
#'
#' @examples
#' equation_to_fit <- tef_change_power4('timeVar',parForm = ~ xvar1*xvar2) # both variables should be numeric for TEfit methods! TEbrm should work with factorsas well

tef_change_power4 <- function(timeVar
                              ,parForm = ~ 1
                              ,startForm = ~ 1
                              ,rateForm= ~ 1
                              ,asymForm= ~ 1
                              ,prevTimeForm= ~ 1
                              ,rateBase=2
                              ,propRemain = .25
){
  ## ## ## TO DO:
  # # - ensure parameterization is exactly as expected
  # # - explain parameterization in docs
  # # - triple check priors are appropriate

  if(is.character(timeVar) || is.factor(timeVar)){

    {
      parForms <- list()

      # # this is to have a single covar for them all (overridden below)
      for(whichPar in c('pStart','pRate','pAsym','pPrevTime')){
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
      if(as.character(prevTimeForm)[2] != '1' || is.numeric(prevTimeForm)){
        parForms[['plogPrevTime']] <- tef_parseParFormula(prevTimeForm, label = 'plogPrevTime')
      }
    }
    nullFun <- tef_parseParFormula(parForm, label = 'null')

    rhs <-  # # #  is parameterized in terms of logX of time to % remaining.
      paste0('pAsym+((pStart)-(pAsym))*((',
             timeVar,'- TIMEVAR_MINIMUM + 1)^(log(',propRemain,
             ')/log(TIMEVAR_MINIMUM +',rateBase,'^(pRate))))*(1/((',rateBase,'^(pPrevTime)+1)^(log(',propRemain,
             ')/log(TIMEVAR_MINIMUM +',rateBase,'^(pRate)))))') ##ISSUE## double check that this parameterization is good, especially with the 'TIMEVAR_LOG_MEDIAN' bits



    rhsString <- rhs
    attr(rhsString,'parForm') <- parForms
    attr(rhsString,'formula') <- rhs
    attr(rhsString,'MEM') <- any(c(
      attr(parForms[['pStart']],'MEM')
      ,attr(parForms[['pRate']],'MEM')
      ,attr(parForms[['pAsym']],'MEM')
      ,attr(parForms[['pPrevTime']],'MEM')
    ))
    attr(rhsString,'changeFun') <- 'power_4par'
    attr(rhsString,'timeVar') <- timeVar
    attr(rhsString,'nullFun') <- nullFun

    attr(rhsString,'changeBase') <- NA
    attr(rhsString,'propRemain') <- propRemain
    attr(rhsString,'rateBase') <- rateBase

    ## ## ISSUE ## ## make sure this isn't just overwritten by the linkfun
    try({
      attr(rhsString,'constantPar_prior') <-
        brms::prior(normal(0,3),nlpar = 'pPrevTime')
    },silent=T)

    if(!attr(rhsString,'MEM')){
      rhsString <- gsub('pStart',attr(parForms[['pStart']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pRate',attr(parForms[['pRate']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pAsym',attr(parForms[['pAsym']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pPrevTime',attr(parForms[['pPrevTime']], "equation"),rhsString,fixed = T)


      ## ## Here, include two vectors as attributes to this vector: first, logical of isIntercept, second, named of whichPar
      ## then can use these later to generate proposal values etc.
      ## should probably have genGuess be a function that is an attribute as well.
      attr(rhsString,'allPars') <- unlist(c(attr(parForms[['pStart']],"parameters"),
                                            attr(parForms[['pRate']],"parameters"),
                                            attr(parForms[['pAsym']],"parameters"),
                                            attr(parForms[['pPrevTime']],"parameters")
      ))

      attr(rhsString,'whichPar') <- c(
        rep('pStart',length(attr(parForms[['pStart']],"parameters"))),
        rep('pRate',length(attr(parForms[['pRate']],"parameters"))),
        rep('pAsym',length(attr(parForms[['pAsym']],"parameters"))),
        rep('pAsym',length(attr(parForms[['pPrevTime']],"parameters")))
      )

      attr(rhsString,'isIntercept') <- grepl('Intercept',attr(rhsString,'allPars'))
    }

    attr(rhs,'change_explanation') <- paste('The change function is a 4-parameter power function.'
                                            ,'It augments the basic 3-parameter power function with the addition of a theoretical'
                                            ,'amount of "previous learning" that is assumed to happen prior to the measured timescale.'
                                            ,'see Heathcote, Brown, & Mewhort (2000), Psychonomic Bulletin & Review, https://doi.org/10.3758/BF03212979')

    # # replace with fixed, if relevant
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
