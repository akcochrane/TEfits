#' Summarize a collection of time-evolving fits
#'
#'
#'
#' @param TEs3s A set of fit TEfit models (output from TEfitAll())
#' @param printOutput Print output to console (if T) or return a list of summary items (if F)
#' @param printAll Print grouping-level fits?
#'
#' @method summary TEfitAll
#' @export
#'

summary.TEfitAll <- function(TEs3s,printOutput=T,printAll=F){
  ### ###

  nPars <- length(TEs3s$allFitList[[1]]$model$par)

  outList <- list(
    model_formula =paste(as.character(TEs3s$allFitList[[1]]$modList$modl_fun[2]),'~',
      as.character(TEs3s$allFitList[[1]]$modList$modl_fun[3]))
    ,param_vals=TEs3s$fitSummary[,1:nPars]
    ,grouping_variable =attr(TEs3s$fitSummary,'grouping_var')
    ,fit_details = TEs3s$fitSummary[,(nPars+1):ncol(TEs3s$fitSummary)]
    ,allFits = TEs3s$allFits
    ,maxRuns = TEs3s$allFitList[[1]]$modList$nTries
    ,convergeTol = TEs3s$allFitList[[1]]$modList$convergeTol
    ,param_product_moment = cor(TEs3s$allFits[,1:nPars])
  )

if(printOutput){
  ### ### Start printing things:
  cat('\n>> Formula:',outList$model_formula)
  cat('\n\n>> Overall effects:\n')
  print(outList$param_vals)
  cat('\n')
  print(outList$fit_details)
if (printAll){
  cat('\n\n>>',attr(TEs3s$fitSummary,'grouping_var'),'- level effects:\n')
  print(outList$allFits)
}
    cat('\n\n>> Max runs:',outList$maxRuns,' -- Tolerance:',outList$convergeTol,
        '\n\n>> Parameter Pearson product-moment correlations:\n')

    round(outList$param_product_moment,3)

  }else{return(outList)}
}
