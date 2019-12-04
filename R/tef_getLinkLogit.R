

#' Delete this, right?
#'
#' @param modList
#'
#' @export
#'
tef_getLinkLogit <- function(modList){
  delete_soon <- T
if(delete_soon){
  ## currently rubbish:
  ## bias default to NONE
  if(!exists('biasAsym',where=modList$covarTerms)){
    modList$covarTerms$biasAsym <- rep(F,length(modList$covars)) }
  if(!exists('biasStart',where=modList$covarTerms)){
    modList$covarTerms$biasAsym <- rep(F,length(modList$covars)) }
  if(!exists('biasRate',where=modList$covarTerms)){
    modList$covarTerms$biasAsym <- rep(F,length(modList$covars)) }

  ## terms, with covariates:
  modList$biasTerms   <- tef_getDatTerms(modList,pPrefix='bias')
  modList$threshTerms <- tef_getDatTerms(modList,pPrefix='thresh')

  ## function strings:
  threshFun <- tef_changeFun(modList,
                             covarVects=list(pA=modList$threshTerms$pA,
                                             pS=modList$threshTerms$pS,
                                             pR=modList$threshTerms$pR))
  biasFun <- tef_changeFun(modList,
                           covarVects=list(pA=modList$biasTerms$pA,
                                           pS=modList$biasTerms$pS,
                                           pR=modList$biasTerms$pR))

  ## make actual logistic, with lapse, etc.:
}

}
