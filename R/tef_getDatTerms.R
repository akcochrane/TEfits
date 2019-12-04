

#' For a TEfit model, use a change function to gt the appropriate RHS terms
#'
#' @param modList List of TEfit model details
#' @param pPrefix {{CREATE EXPLANATION}}
#' @param whichChange {{CREATE EXPLANATION}}
#'
#' @export
#'
tef_getDatTerms <- function(modList,pPrefix,whichChange='expo'){

  if(whichChange=='expo'){datTerms <- tef_getTermsExpo(modList,pPrefix)}

  if(whichChange=='power'){datTerms <- tef_getTermsExpo(modList,pPrefix)}

  if(whichChange=='power4'){datTerms <- tef_getTermsPower4(modList,pPrefix)}

  if(whichChange=='weibull'){datTerms <- tef_getTermsWeibull(modList,pPrefix)}

  if(whichChange=='expo_block'){
    datTerms <- tef_getTermsExpoBlock(modList,pPrefix)
  }

  if(whichChange=='expo_double'){
    datTerms <- tef_getTermsExpoDouble(modList,pPrefix)
  }

  if(whichChange=='expo_fatigue'){
    datTerms <- tef_getTermsExpoFatigue(modList,pPrefix)
  }

  if(!exists('datTerms')){cat('\nNo data terms. Did you specify a change function properly?\n')}
  return(datTerms)
}
