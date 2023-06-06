
#' Construct a formula with basis functions over time
#' 
#' Adds basis functions, over the dimension of time, to a 
#' conventional [generalized] linear mixed-effects model formula.
#' 
#' Within a typical [G]LMEM, adding basis functions over the dimension
#' of time can control for time-related variations, while also allowing 
#' for the recovery of those variations. This function takes in the
#' formula of interest (e.g., \code{y ~ x + (x|ID)}) and its associated
#'  time data, and it returns an augmented formula 
#'  (e.g., \code{y ~ x + (x|ID) + (0 + basis_0 + basis_1 + basis_2 + basis_3 || ID)}).
#'  The corresponding data (i.e., constructed bases) are included as an attribute of the 
#'  returned formula (i.e., \code{returnedFormula,'dfOut'}).
#'  
#'  This function is designed to be compatible with \code{lme4} and 
#'  \code{brms} models.
#'
#' @param formula_mem Formula, specified as in \code{brm} or \code{glmer}
#' @param ... 
#' @param groupingVar Character. Grouping variable for each timecourse (e.g., participant ID, or combination of participant and session)
#' @param timeVar Numeric vector. Variable over which basis functions are to be constructed.
#' @param basisDens Numeric scalar. Width between basis functions peaks (e.g., distance between first and second basis function highest-density points)
#' @param basis_calc_fun Character. Function by which the basis functions are constructed. Currently only "gaussian" is implemented.
#'
#' @export
#'
time_basisFun_formula <- function(formula_mem
                                  ,...
                                  ,groupingVar
                                  ,timeVar
                                  ,basisDens
                                  , basis_calc_fun='gaussian'){
  
  if(F){ # for testing
    d <- iris
    formula_mem <- Sepal.Length ~ Petal.Length 
    d$timeVar <- timeVar <- rep(1:50,3)
    basisDens <- 10
    basis_calc_fun='gaussian'
    groupingVar <- 'Species'
  }
  
  basis_df <- time_basisFun_df(timeVar = timeVar
                               ,basisDens = basisDens
                               ,basis_calc_fun = basis_calc_fun)
  
  formOut <- as.formula(
    paste0(as.character(formula_mem)[2] , ' ~ '
           ,as.character(formula_mem)[3],' + (0 + '
           ,paste(colnames(basis_df)[2:ncol(basis_df)],collapse='+')  
           ,' || ',groupingVar,')'
    )
  )
  
  attr(formOut,'dfOut') <- basis_df
  
  return(formOut)  
}
