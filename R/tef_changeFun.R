#' Construct the right hand side of the equation for a TEfit
#'
#' @param modList List of TEfit model details
#' @param covarVects Vectors of covariate(s) for each model parameter
#'
#' @export
#'
tef_changeFun <- function(modList,covarVects){
timeVar_origin <- min(modList$varIn[,modList$timeVar],na.rm=T)

  if(modList$changeFun=='expo'){
 change_fun <-  paste0('((',covarVects$pA,')+((',covarVects$pS,')-(',covarVects$pA,'))*',
                       modList$expBase,'^((',timeVar_origin,'-',modList$timeVar,')/(',modList$rateBase,'^(',covarVects$pR,'))))'
  )

  }

  if(modList$changeFun=='expo_double'){
    change_fun <-  paste0('((',covarVects$pA,')+((',covarVects$pS,')-(',covarVects$pA,'))*.5*',
                          modList$expBase,'^((',timeVar_origin,'-',modList$timeVar,')/(',modList$rateBase,'^(',covarVects$pRa,')))',
                          '+((',covarVects$pS,')-(',covarVects$pA,'))*.5*',
                          modList$expBase,'^((',timeVar_origin,'-',modList$timeVar,')/(',modList$rateBase,'^(',covarVects$pRb,'))))'
    )
  }

  if(modList$changeFun=='expo_block'){
    change_fun <-  paste0('((',covarVects$pA,')+((',covarVects$pS,')-(',covarVects$pA,'))*',
                          modList$expBase,'^((',timeVar_origin,'-',modList$timeVar,')/(',mFit$modList$rate_lim[1],'+',
                          modList$rateBase,'^(',covarVects$pR,')))*',
                          '(1+',covarVects$pBS,'*',modList$expBase,'^((',timeVar_origin,'-',
                          modList$blockTimeVar,')/(',
                          modList$rateBase,'^',covarVects$pBR,'))))'
    )
  }

  if(modList$changeFun=='expo_fatigue'){
    change_fun <-  paste0('((',covarVects$pA,') + ((',covarVects$pS,')-(',covarVects$pA,'))*',
                          modList$expBase,'^((',timeVar_origin,'-',modList$timeVar,')/(',
                          modList$rateBase,'^(',covarVects$pR,'))) + ',
                          '(',covarVects$pFatigueAsym,'-',covarVects$pA,')*(1/',
                          '(1+3^(((', covarVects$pFatigueTime,')-',modList$timeVar,')/(',
                          covarVects$pFatigueHWHM,')))))'
    )
  }

  if(modList$changeFun=='power'){
    change_fun <-  # # #  is parameterized in terms of logX of time to % remaining.
    paste0('((',covarVects$pA,')+((',covarVects$pS,')-(',covarVects$pA,'))*(',
           modList$timeVar,'-',timeVar_origin-1,')^(log(',modList$propRemain,
           ')/log(',modList$rateBase,'^',covarVects$pR,')))')
  }

  if(modList$changeFun=='power4'){
    change_fun <-
      paste0('((',covarVects$pA,')+((',covarVects$pS,')-(',covarVects$pA,'))*(((',
             modList$timeVar,'-',timeVar_origin-1,')+',covarVects$pPrevTime,')^(log(',modList$propRemain,
             ')/log(',modList$rateBase,'^(',covarVects$pR,'))))*(1/((',covarVects$pPrevTime,'+1)^(log(',modList$propRemain,
             ')/log(',modList$rateBase,'^(',covarVects$pR,'))))))')
  }

  if(modList$changeFun=='weibull'){
    # expBase defines time constant; invert expBase to find percent of change completed, e.g., 2^(-1)=.5 of change remaining
    # which means that, like exponential, pR is log(,rate=rateBase) of time to expBase^(-1) of change remaining
    # Then, pShape is the [scaled log(,base=expBase) of the] slope of the function at expBase^(-1)? more or less
    change_fun <-  paste0('((',covarVects$pA,')+((',covarVects$pS,')-(',covarVects$pA,'))*',
                          modList$expBase,'^(-((',modList$timeVar,'-',timeVar_origin,')/(',modList$rateBase,'^(',covarVects$pR,')))^(',modList$expBase,'^',
                          covarVects$pShape,')))'
    )
  }

  return(change_fun)
}
