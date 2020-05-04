#' Summarize a time-evolving fit
#'
#' Prints or returns a summary of a TEfit object. This includes parameter values,
#' convergence, the full formula, goodness-of-fit metrics, measures of change in
#' conditional independence, and (if applicable) distributional information from resampling.
#'
#' Pseudo-SE is an approximation to the standard error of the parameter using bootstrapped estimates. It is calculated by
#' first calculating the .025 and .975 quantile() of the resampled parameters. Then, the absolute
#' difference is calculated between each of these CI values and the overall fit value. The absolute
#' differences are divided by qnorm(.975) and averaged in order to get the pseudo-SE, the
#' pseudo standard deviation of the parameter expected value.
#'
#' @param TEs3 A fit TEfit model
#' @param printOutput Print output to console (if T) or return a list of summary items (if F)
#'
#' @method summary TEfit
#' @export
#'
#' @examples
#' \dontrun{summary(model_fit_by_TEfit)}
#'
summary.TEfit <- function(TEs3,printOutput=T){
  ### ###

  outList <- list(
  param_vals = coef(TEs3)
  ,convergence = TEs3$model$converged
  ,model_formula = paste0(as.character(TEs3$modList$modl_fun[2]),'~',as.character(TEs3$modList$modl_fun[3]))
  ,GoF = TEs3$model$GoF
  ,conditional_independence=TEs3$model$conditional_independence
  )



  if(exists('bootList',TEs3)){

    outList$percent_resamples_posSlope <- round(100*TEs3$bootList$percent_increasing,1)
    outList$timepoint_divergence <- TEs3$bootList$time_d_over_1
    outList$resampled_correlations <- round(TEs3$bootList$bootCorrel[
      1:(nrow(TEs3$bootList$bootCorrel)-3),
      1:(ncol(TEs3$bootList$bootCorrel)-3)],3)
  }

  if(printOutput){
  ### ### Start printing things:
  cat('\n>> Call:',outList$model_formula)
  cat('\n\n>> Converged:',outList$convergence,'\n')
  if(!TEs3$model$converged){
    cat('>> Max runs:',TEs3$modList$nTries,' -- Tolerance:',TEs3$modList$convergeTol,'\n')
  }
  cat('\n>> Fit Values:\n')
  print(outList$param_vals)
  cat('\n>> Goodness-of-fit:\n')
  print(outList$GoF)
  if(TEs3$modList$changeFun=='power4'){
    if(TEs3$modList$linkFun$link != 'weibull' && TEs3$modList$linkFun$link != 'logit'){
      cat('\n>> Predicted starting value, prior to measurements:',round(TEs3$model$learning_start_value,2),'\n')
    }else{
      cat('\n>> Predicted starting threshold, prior to measurements:',round(TEs3$model$learning_start_thresh,2),'\n')
    }

  }
  if(TEs3$modList$linkFun$link != 'weibull' && TEs3$modList$linkFun$link != 'logit'){
  cat('\n>> Test of change in nonindependence:\n')
  print(outList$conditional_independence)
}
  if(exists('bootList',TEs3)){

    if(TEs3$bootList$bootPercent < 1){
      bTitle <- 'Subsampled'
      cat('\n>> >Note: Subsampled parameter distributions are likely to be inaccurate. If the goal is to interpret resampled parameter CI, SE or correlations, resampling with replacement is recommended.<\n')
    }else{bTitle <- 'Bootstrapped'}

    cat("\n>> Percent of resamples predicting an increase in values:",outList$percent_resamples_posSlope,'\n')
    cat("\n>> Timepoint at which resampled estimates diverge from timepoint 1, with Cohen's d>1:",outList$timepoint_divergence,'\n')
    cat('\n>>',bTitle,'parameter correlations:\n')
    print(outList$resampled_correlations)
  }

  }else{return(outList)}
}
