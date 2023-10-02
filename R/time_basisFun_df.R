
#' Construct basis functions
#'
#' Given a numeric time vector, construct a set of overlapping basis functions.
#'
#' Bases are equally-spaces with respect to time. Together, entered as linear predictors
#' into a typical regression model, each basis function acts as a "bump" offset from the
#' overall model fit, thereby approximating arbitrary nonlinear changes over time.
#'
#' The function defining the bases defaults to Gaussian with a half-width half-max equal
#' to the distance between the basis centeres, that is, each basis function
#' uses appxroximately \code{dnorm(time_vector,basis_center,basis_width / .5875)}.
#' The bases are then normalized so that each timepoint's bases sum to 1.
#'
#' @param timeVar Vector of time (e.g., trial number)
#' @param basisDens Space between basis function peaks
#' @param basis_calc_fun The function with which to calculate the basis functions.
#'
#' @export
#'
time_basisFun_df <- function(timeVar,basisDens, basis_calc_fun='gaussian'){

  if(is.character(basis_calc_fun)){
    constAdj <- sqrt(2 * log(2)) /2
    calc_fun <- switch(basis_calc_fun
                       ,gaussian = function(...,width){dnorm(...,sd=width/constAdj )}
    )
  }else{
    calc_fun <- basis_calc_fun
  }

  basisDF_ref <- data.frame(time_orig = sort(unique(na.omit(timeVar))))

  basisCenters <- seq(min(basisDF_ref$time_orig) - (basisDens+1)
                      ,max(basisDF_ref$time_orig) + basisDens
                      ,basisDens)
  basisNamePrec <- 3 - nchar(signif(max(basisCenters) - min(basisCenters),3))

  for(curBasisCenter in basisCenters){
    basisDF_ref[,paste0('basis_cen_',round(curBasisCenter,basisNamePrec))] <-
      calc_fun(basisDF_ref$time_orig, curBasisCenter, width = basisDens/2)
  }

  colnames(basisDF_ref) <- gsub('-','M', colnames(basisDF_ref),fixed = T)

  basisDF_ref[,2:ncol(basisDF_ref)] <-
    basisDF_ref[,2:ncol(basisDF_ref)] / rowSums(basisDF_ref[,2:ncol(basisDF_ref)])

  timeOut <- data.frame(order_orig = 1:length(timeVar),time_orig = timeVar)

  timeOut <- merge(timeOut,  basisDF_ref, by='time_orig')

  timeOut <- timeOut[order(timeOut$order_orig),]

  timeOut$order_orig <- NULL

  attr(timeOut,'basis_centers') <- basisCenters

  return(timeOut)
}
