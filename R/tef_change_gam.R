#' Construct a by-time Generalized Additive Model formula
#'
#' By defining the model variable associated with time (e.g., trial number), and
#' formulas defining each of the nonlinear parameters of time-related change,
#' this function constructs a model that can then be passed to functions for fitting
#' the model (e.g., \code{\link{TEbrm}}).
#'
#' Use is primarily for passing to link functions. If no link function is needed, then an equivalent
#' formula can be passed directly to \code{\link[brms]{brm}}.
#'
#' Function is \strong{under development}
#' and is likely to be buggy, and to change frequently.
#'
#' @param timeVar String. Indicates the name of the time variable
#' @param timeCovar Vector of strings, for variables that will be included in the time-evolving model.
#' @param groupingVar String. Optional grouping variable (e.g., participant ID, in a behavioral study). If this is non-empty, then all \code{timeCovar} are fit on the level of \code{groupingVar}
#' @param bs Two letter character string inidicating the basis for smoothing. See ?mgcv::s
#'
#' @seealso
#' \code{\link{TEbrm}} for examples of how to use this function in specifying models.
#'
#' @export
#'
#' @examples
#' equation_to_fit <- tef_change_gam('trialNum')
#'
#'
tef_change_gam <- function(timeVar
                             ,timeCovar = c(NULL)
                             ,groupingVar = ''
                           # ,k = NA
                           ,bs = c('ts','cr','cs','tp','cc')
){

  if(length(bs)>1){bs=bs[1]}

  k <- -1

  ## >> NEED TO TEST! Also include covariates / linear predictors / explanations of interpretations?

  ## >> possibly implement a gamlss() version?

    if(is.character(timeVar) || is.factor(timeVar)){


      ## for testing!!
        if(F){
    library(TEfits) ; library(brms)
d <- anstrain
d$absRat <- abs(d$ratio)
  mPart <- brm(acc ~ absRat+sizeRat + s(trialNum,by=subID) + (absRat+sizeRat||subID),data = d )
  mFull <- brm(acc ~ s(absRat+sizeRat + trialNum,by=subID) + (absRat+sizeRat||subID),data = d )
  summary(m)

  timeVar <- 'trialNum'
  timeCovar <- 'absRat'
  groupingVar <- 'subID'



  m3 <- brm(
    as.formula(paste('acc ~',as.character(rhs)[[2]]))
    ,d
            )
  }

    # {
    #   parForms <- list()
    #
    #   # # this is to have a single covar for them all (overridden below)
    #   for(whichPar in c('pStart','pRate','pAsym')){
    #     parForms[[whichPar]] <-  as.formula( tef_parseParFormula(parForm, label = whichPar) )
    #   }
    #
    #   # # for specific covars
    #   if(as.character(startForm)[2] != '1' || is.numeric(startForm)){
    #     parForms[['pStart']] <- tef_parseParFormula(startForm, label = 'pStart')
    #   }
    #
    #   if(as.character(rateForm)[2] != '1' || is.numeric(rateForm)){
    #     parForms[['pRate']] <- tef_parseParFormula(rateForm, label = 'pRate')
    #
    #   }
    #
    #   if(as.character(asymForm)[2] != '1' || is.numeric(asymForm)){
    #     parForms[['pAsym']] <- tef_parseParFormula(asymForm, label = 'pAsym')
    #
    #   }
    # }



      if(nchar(groupingVar)==0){
        rhs <-
          # as.formula(
          paste0(
          '',
          paste(c(1,timeCovar),collapse='+',sep='+')
          ,' + s('
          ,paste(c(timeCovar,timeVar),collapse=',',sep=',')
          ,',k=',k,',bs="',bs,'")'
        )
        # )

        # m4 <- brm(
        #   as.formula(paste('acc ~',as.character(rhs)[[2]]))
        #   ,d
        # )

      }else{
        rhs <-
          # as.formula(
          paste0(
          ' ',
          ' s('
          ,paste(c(timeVar,timeCovar),collapse=',',sep=',')
          ,',by='
          ,groupingVar
          ,',k=',k,',bs="',bs,'") + '
          ,'('
          , paste(c(1,timeCovar),collapse='+',sep='+')
          ,' | ',groupingVar,')'
          # )
        )
      }

      ### ### ###
      ### ## ###


    nullFun <- 'NEED STILL' # tef_parseParFormula(parForm, label = 'null')

    # rhs <- paste0(
    #   'pAsym + ( ('
    #   ,'pStart)-(',
    #   'pAsym) )*',
    #   changeBase,'^( (TIMEVAR_MINIMUM-',
    #   timeVar,') / (',
    #   rateBase,'^(pRate) ) )'
    # )

    rhsString <- rhs
    attr(rhsString,'parForm') <- 'STILL NEED' #parForms
    attr(rhsString,'formula') <- rhs
    attr(rhsString,'MEM') <- nchar(groupingVar)==0

        #   any(c(
    #   attr(parForms[['pStart']],'MEM')
    #   ,attr(parForms[['pRate']],'MEM')
    #   ,attr(parForms[['pAsym']],'MEM')
    # ))

    attr(rhsString,'changeFun') <- 'GAM'
    attr(rhsString,'timeVar') <- timeVar
    attr(rhsString,'nullFun') <- nullFun

    attr(rhsString,'changeBase') <- NA # changeBase
    attr(rhsString,'rateBase') <- NA #rateBase

    if(F){ # if(!attr(rhsString,'MEM')){
      rhsString <- gsub('pStart',attr(parForms[['pStart']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pRate',attr(parForms[['pRate']], "equation"),rhsString,fixed = T)
      rhsString <- gsub('pAsym',attr(parForms[['pAsym']], "equation"),rhsString,fixed = T)

      ## ## Here, include two vectors as attributes to this vector: first, logical of isIntercept, second, named of whichPar
      ## then can use these later to generate proposal values etc.
      ## should probably have genGuess be a function that is an attribute as well, that takes the asymlink, the LHS dist, etc.
      attr(rhsString,'allPars') <- ''
      # unlist(c(attr(parForms[['pStart']],"parameters"),
      #                                       attr(parForms[['pRate']],"parameters"),
      #                                       attr(parForms[['pAsym']],"parameters")))

      attr(rhsString,'whichPar') <- ''
      # c(
      #   rep('pStart',length(attr(parForms[['pStart']],"parameters"))),
      #   rep('pRate',length(attr(parForms[['pRate']],"parameters"))),
      #   rep('pAsym',length(attr(parForms[['pAsym']],"parameters")))
      # )

      attr(rhsString,'isIntercept') <- grepl('Intercept',attr(rhsString,'allPars'))
    }

    # # replace with fixed, if relevant ##ISSUE## Need to put this into Weibull
    # for(curPar in 1:length(attr(rhsString, 'parForm'))){
    #   if(attr(attr(rhsString, 'parForm')[[curPar]],'is_fixed')){
    #     attr(rhsString,'formula') <- gsub(names(attr(rhsString, 'parForm'))[curPar]
    #                                       ,as.numeric(attr(rhsString, 'parForm')[[curPar]])
    #                                       ,attr(rhsString,'formula'),fixed = T)
    #   }
    # }

    return(
      rhsString
    )
  }else{stop('The timeVar argument should be the name of the time variable.')}
}
