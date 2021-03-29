#' Construct a 3-parameter exponential function of change
#'
#' By defining the model variable associated with time (e.g., trial number), and
#' formulas defining each of the nonlinear parameters of time-related change,
#' this function constructs a model that can then be passed to functions for fitting
#' the model (e.g., \code{\link{TEbrm}}).
#'
#' Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' @param timeVar String. The name of the variable in the model that corresponds to time. The variable of time should be positive and numeric, and the function of change should be expected to happen with increasing time. \emph{Extensive testing has only been done on positive integer time variables with a minimum time of 1.}
#' @param parForm The right-hand side of the formula defining all nonlinear parameters as well as the null [non-time-varying] model.
#' @param startForm The right-hand side of the formula defining the start parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter.
#' @param rateForm The right-hand side of the formula defining the rate parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter.
#' @param asymForm The right-hand side of the formula defining the asymptote parameter. If anything besides \code{~1}, overwrites \code{parForm} for this parameter.
#' @param changeBase Number. The base of the log (e.g., 2 or exp(1)) of the change function.
#' @param rateBase  Number. The base of the log (e.g., 2 or exp(1)) of the rate [time constant].
#'
#' @seealso
#' \code{\link{TEbrm}} for examples of how to use this function in specifying models.
#'
#' @export
#'
#' @examples
#' equation_to_fit <- tef_change_expo3('timeVar',rateForm = ~ xvar1*xvar2) # both variables should be numeric for TEfit methods! TEbrm should work with factorsas well
#'
#' equation_to_fit <- tef_change_expo3('timeVar'
#' , parForm = ~ xvar1   # overall parameter formula; overwritten for time-evolving formulas below, leaving this as just the null model
#' , startForm = ~ xvar2 # start parameter's regression model
#' , rateForm = ~ (1|participantID)  # rate parameter's [mixed-effects] regression model
#' , asymForm = ~ xvar3 # asymptote parameter's regression model
#' )
#'
tef_change_expo3 <- function(timeVar
                             ,parForm = ~ 1
                             ,startForm = ~ 1
                             ,rateForm= ~ 1
                             ,asymForm= ~ 1
                             ,changeBase=2
                             ,rateBase=2
){
  if(is.character(timeVar) || is.factor(timeVar)){

    {
      parForms <- list()

      # # this is to have a single covar for them all (overridden below)
      for(whichPar in c('pStart','pRate','pAsym')){
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
    }

    nullFun <- tef_parseParFormula(parForm, label = 'null')

    rhs <- paste0(
      'pAsym + ( ('
      ,'pStart)-(',
      'pAsym) )*',
      changeBase,'^( (TIMEVAR_MINIMUM-',
      timeVar,') / (',
      rateBase,'^(pRate) ) )'
    )

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

    attr(rhsString,'changeBase') <- changeBase
    attr(rhsString,'rateBase') <- rateBase

    if(!attr(rhsString,'MEM')){
      rhsString <- gsub('pStart',attr(parForms[['pStart']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pRate',attr(parForms[['pRate']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pAsym',attr(parForms[['pAsym']], "equation"),rhsString,fixed = T)

      ## ## Here, include two vectors as attributes to this vector: first, logical of isIntercept, second, named of whichPar
      ## then can use these later to generate proposal values etc.
      ## should probably have genGuess be a function that is an attribute as well, that takes the asymlink, the LHS dist, etc.
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
  }else{stop('The timeVar argument should be the name of the time variable.')}
}
