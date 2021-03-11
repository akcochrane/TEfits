#' Construct a 3-parameter power function of change
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
#' @param rateBase  Number. The base of the log (e.g., 2 or exp(1)) of the rate [time constant].
#' @param propRemain parameterized in terms of logX of time to % remaining.
#'
#' @seealso
#' \code{\link{TEbrm}} for examples of how to use this function in specifying models.
#'
#' @export
#'
#' @examples
#' equation_to_fit <- tef_change_power3('timeVar',rateForm = ~ xvar1*xvar2) # both variables should be numeric for TEfit methods! TEbrm should work with factorsas well
#'
#' equation_to_fit <- tef_change_power3('timeVar'
#' , parForm = ~ xvar1   # overall parameter formula; overwritten for time-evolving formulas below, leaving this as just the null model
#' , startForm = ~ xvar2 # start parameter's regression model
#' , rateForm = ~ (1|participantID)  # rate parameter's [mixed-effects] regression model
#' , asymForm = ~ xvar3 # asymptote parameter's regression model
#' )
#'
tef_change_power3 <- function(timeVar
                             ,parForm = ~ 1
                             ,startForm = ~ 1
                             ,rateForm= ~ 1
                             ,asymForm= ~ 1
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
      for(whichPar in c('pStart','pRate','pAsym')){
        parForms[[whichPar]] <-  as.formula( tef_parseParFormula(parForm, label = whichPar) )
      }

      # # for specific covars
      if(as.character(startForm)[2] != '1'){
        parForms[['pStart']] <- tef_parseParFormula(startForm, label = 'pStart')
      }
      if(as.character(rateForm)[2] != '1'){
        parForms[['pRate']] <- tef_parseParFormula(rateForm, label = 'pRate')
      }
      if(as.character(asymForm)[2] != '1'){
        parForms[['pAsym']] <- tef_parseParFormula(asymForm, label = 'pAsym')
      }
    }
    nullFun <- tef_parseParFormula(parForm, label = 'null')

    rhs <-  # # #  is parameterized in terms of logX of time to % remaining.
      paste0('(pAsym+((pStart)-(pAsym))*(',
             timeVar,'- TIMEVAR_MINIMUM + 1)^(log(',propRemain,
             ')/log(',rateBase,'^pRate)))')

    rhsString <- rhs
    attr(rhsString,'parForm') <- parForms
    attr(rhsString,'formula') <- rhs
    attr(rhsString,'MEM') <- any(c(
      attr(parForms[['pStart']],'MEM')
      ,attr(parForms[['pRate']],'MEM')
      ,attr(parForms[['pAsym']],'MEM')
    ))
    attr(rhsString,'changeFun') <- 'exponential_3par'
    attr(rhsString,'timeVar') <- timeVar
    attr(rhsString,'nullFun') <- nullFun

    attr(rhsString,'changeBase') <- NA
    attr(rhsString,'propRemain') <- propRemain
    attr(rhsString,'rateBase') <- rateBase

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

    return(
      rhsString
    )
  }
}
