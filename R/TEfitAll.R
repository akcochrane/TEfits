
#' Fit several time-evolving regression models
#'
#' A wrapper for fitting a \code{\link{TEfit}} model to the data
#' for every unique value of groupingVar. Defaults to
#' returning a list including two summaries and all models; returning
#' only a summary is also an option. Arguments (except groupingvar, a grouping vector)
#' are identical to, and are passed directly to, \code{\link{TEfit}}.
#'
#' @inheritParams TEfit
#' @param groupingVar Variable (e.g., participant ID) with which to separate TEfit models. Length must be nrows(varIn)
#' @param groupingVarName Name of grouping var
#' @param returnAll Logical. Return only a summary (when T), or that summary plus every model, in a list (when F)
#' @param progressDot If TRUE, prints a dot after each group fit
#'
#' @seealso
#' \code{\link{TEfit}} for fitting a single model;
#' \code{\link{tef_fitAll2brms}} to re-fit the TEfitAll output using \code{\link[brms]{brms}}
#'
#' @export
#'
TEfitAll <- function(varIn,
                     groupingVar,
                     groupingVarName = 'grouping_var',
                     returnAll=T,
                     progressDot=T,
                     linkFun = list(link='identity'),
                     errFun = 'ols',
                     changeFun = 'expo',
                     bootPars = list(nBoots = 0, bootTries = 0, bootPercent=0),
                     blockTimeVar = NULL,
                     covarTerms = list(),
                     control=tef_control()
){

  TEFitList <- list()
  TEFit_group <- data.frame()
  fit_data <- data.frame()

  for(curGroup in unique(groupingVar)){
    TEFitList[[length(TEFitList)+1]] <- TEfit(
      varIn=varIn[groupingVar==curGroup,],linkFun=linkFun,errFun=errFun,
      changeFun=changeFun,bootPars=bootPars,
      blockTimeVar=blockTimeVar,
      covarTerms=covarTerms,control=control
    )

    summLine <- TEFitList[[length(TEFitList)]]$model$par
    if(bootPars$nBoots>0){
      pseudoSEs <- as.vector(coef(TEFitList[[length(TEFitList)]])$pseudoSE)
      names(pseudoSEs) <- paste0(names(summLine),'_pseudoSE')

      percentIncreasing <- TEFitList[[length(TEFitList)]]$bootList$percent_increasing
      names(percentIncreasing) <- 'percent_samples_increasing'

      summLine <- c(summLine,pseudoSEs,percentIncreasing)
    }

    TEFit_group <- rbind(TEFit_group,c(
      summLine,
      TEFitList[[length(TEFitList)]]$model$GoF,
      linkFun=linkFun$link,errFun=errFun,changeFun=changeFun,
      converged=TEFitList[[length(TEFitList)]]$model$converged,
      pValSpearmanChange=as.numeric(TEFitList[[length(TEFitList)]]$model$conditional_independence['pValSpearmanChange'])
    ))
    rownames(TEFit_group)[nrow(TEFit_group)] <- curGroup

    fit_data <- rbind(fit_data,TEFitList[[length(TEFitList)]]$data)

    if(progressDot){cat('. ')}

  }

  ## ## ## ## get the overall values, across groupVar subsets
  TEFitSummary <- data.frame(summaryStat=c('mean','stdErr'))
  for(curCol in 1:ncol(TEFit_group)){
    if(is.numeric(TEFit_group[,curCol]) || is.logical(TEFit_group[,curCol])){
      TEFitSummary <- cbind(TEFitSummary,
                            c(mean(TEFit_group[,curCol]),
                              sd(TEFit_group[,curCol])/sqrt(nrow(TEFit_group)))
      )
    }else(TEFitSummary <- cbind(TEFitSummary,unique(TEFit_group[,curCol])))
  }
  rownames(TEFitSummary) <- TEFitSummary$summaryStat
  TEFitSummary <- TEFitSummary[,2:ncol(TEFitSummary)]
  colnames(TEFitSummary) <- colnames(TEFit_group)

  attr(TEFitSummary,'grouping_var') <- groupingVarName
  attr(TEFit_group,'grouping_var') <- groupingVarName
  attr(TEFitList,'grouping_var') <- groupingVarName

  if(returnAll){
    outList <- list(fitSummary=TEFitSummary ,allFits = TEFit_group,allFitList = TEFitList,data=fit_data)# return everything that was calculated

    class(outList) <- 'TEfitAll'
    return(outList)
  }else(return(TEFit_group)) # return only the by-groupVar fits
}
