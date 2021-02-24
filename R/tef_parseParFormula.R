
#' Parse a parameter-level formula for a TEfit model
#' 
#' Returns the \code{parFormula}, with some additional information that assists TEfits in fitting.
#' 
#' If there are no random-effects terms, parses into a literal equation (e.g., turns 
#' \code{~ x1 + x2 * x3} into \code{pPar_Intercept + pPar_x1\*x1 + pPar_x2\*x2 + pPar_x3\*x3 + pPar_x2_x3\*x2\*x3}) 
#' which is returned as the "equation" attribute that has an accompanying "parameters" attribute. Additional attributes
#' include whether there are random-effects terms (attribute "MEM") and whether there are any parameters at all, or if the
#' "equation" is actually fixed to a variable or scalar (attributed "is_fixed").
#'
#' @param parFormula Formula for fitting a parameter. Variables should be numeric; non-numeric variables may break. Can also take a string or a scalar, if the parameter is to be fixed to that value.
#' @param label Label for the parameters
#'
#' @export
#'
#' @examples
#' tef_parseParFormula(~ x1 + x2*x3 + (x2 || subID))
#' tef_parseParFormula(~ x1 + x2*x3, label = 'thresholdAsymptote')
#' tef_parseParFormula(2)
#' 
tef_parseParFormula <- function(parFormula,label = 'pPar'){
  
  # parFormula <- ~ x1 + x2*x3 + (x2 || subID)
  # parFormula <- ~ x1 + x2*x3
  ## last thing: defie default behavior!
  
  if(!any(grep('~',parFormula,fixed=T))){
    attr(parFormula,'equation') <- parFormula
    attr(parFormula,'is_fixed') <- T
    attr(parFormula,'parameters') <- c()
    
  }else{  
    attr(parFormula,'is_fixed') <- F
    
    rhsTerms <- attr(terms(parFormula),'term.labels')
    
    attr(parFormula,'MEM') <- any(grep('|',rhsTerms,fixed=T))
    
    if(!attr(parFormula,'MEM')){
      
      rhsTerms_vars <- gsub(':','*',rhsTerms,fixed=T)
      rhsTerms_pars <- gsub(':','_',rhsTerms,fixed=T)
      
      attr(parFormula,'parameters') <- paste(label,c('Intercept',rhsTerms_pars),sep='_')
      
      nonIntercept <- paste0(rhsTerms_pars,'*',rhsTerms_vars)
      if(nonIntercept[1] == '*'){nonIntercept <- c()}
      
      attr(parFormula,'equation') <- 
        paste(paste(label,c('Intercept',nonIntercept),sep='_'),collapse=' + ')
    }
  }
  return(parFormula)  
}