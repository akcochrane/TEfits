
#' Fit several time-evolving regression models
#'
#' A wrapper for fitting a TEfit model to the data
#' for every unique value of groupingVar. Defaults to
#' returning a list including two summaries and all models; returning
#' only a summary is also an option.
#'
#' @param varIn   Data frame or vector. First column [or vector] is the time-dependent response variable. If available, second column is the time variable. All other columns are covariates, possibly involved in a link function.
#' @param groupingVar Variable (e.g., participant ID) with which to separate TEfit models. Length must be nrows(varIn)
#' @param returnAll Logical. Return only a summary (when T), or that summary plus every model, in a list (when F)
#' @param progressDot If TRUE, prints a dot after each group fit
#' @param linkFun A list defining a link function (i.e., 'identity', 'Quick', or 'logistic')
#' @param errFun  A string defining an error function (e.g., 'ols', 'logcosh', 'exgauss'). Full description {here}
#' @param changeFun A string defining the functional form of change (e.g., 'expo', 'power', 'weibull')
#' @param bootPars A list defining the details for bootstrapped fits. Defaults to no bootstrapping. Necessary for estimates of uncertainty around fits and for covariance between parameters.
#' @param blockTimeVar A string defining the time points of sub-scales (e.g., "blocks" of times within the overall timescale of data collection)
#' @param covarTerms {{description}}
#' @param control A list of model parameters. Full description {here}
#'
#' @export
#'
TEfitAll <- function(varIn,
                     groupingVar,
                     returnAll=T,
                     progressDot=T,
                  linkFun = list(link='identity'),
                  errFun = 'ols',
                  changeFun = 'expo',
                  bootPars = list(nBoots = 0, bootTries = 0, bootPercent=0),
                  blockTimeVar = NULL,
                  covarTerms = list(),
                  control=list()
){

  TEFitList <- list()
  TEFit_group <- data.frame()

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

  ## ## get the actual grouping variable name, if used
  if(!is.null(names(groupingVar))){
     grouping_variable <-
      unique(substr(names(groupingVar),1,min(nchar(names(groupingVar))-1)))
  }else{grouping_variable <- 'grouping_variable'}
  attr(TEFitSummary,'grouping_var') <- grouping_variable
  attr(TEFit_group,'grouping_var') <- grouping_variable
  attr(TEFitList,'grouping_var') <- grouping_variable



if(returnAll){
  outList <- list(fitSummary=TEFitSummary ,allFits = TEFit_group,allFitList = TEFitList)# return everything that was calculated
  class(outList) <- 'TEfitAll'
  return(outList)
}else(return(TEFit_group)) # return only the by-groupVar fits
}

if(F){
library(TEfits)
d <- data.frame(y=c(seq(0,3,length=25),rep(3,length=25),
                    seq(0,4,length=25),rep(4,length=25),
                    seq(1,3,length=25),rep(3,length=25)
                    )+rnorm(150)+3,
                x =c(1:50,1:50,1:50),
                sID = c(rep(c('a','b','c'),each=50)))
m <- TEfitAll(d[,1:2],d[,3],bootPars=list(nBoots=20,bootPercent=.8),errFun='logcosh')
summary(m)
}
