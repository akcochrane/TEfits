
#' Construct basis functions
#'
#' Given a numeric time vector, construct a set of overlapping basis functions.
#'
#' Gaussian bases are equally-spaced with respect to time. Together, entered as linear predictors
#' into a typical regression model, each basis function acts as a "bump" offset from the
#' overall model fit, thereby approximating arbitrary nonlinear changes over time.
#'
#' The function defining the bases defaults to Gaussian with a half-width half-max equal
#' to the distance between the basis centers, that is, each basis function
#' uses approximately \code{dnorm(time_vector,basis_center,basis_width / .5875)}.
#' The bases are then normalized so that each timepoint's bases sum to 1.
#'
#' B spline bases are also implemented (and are more conventional), relying on the \code{fda} package.
#'
#' @param timeVar Vector of time (e.g., trial number)
#' @param basisDens Space between basis function peaks (equal if gaussian; approximate if bspline)
#' @param basis_calc_fun The function with which to calculate the basis functions. Either \code{bspline} (relies on \code{fda} package) or \code{gaussian}.
#'
#' @export
#'
time_basisFun_df <- function(timeVar,basisDens, basis_calc_fun='gaussian'){

  # so basically, it's unlikely to be easy to shoehorn the gaussian and bspline together
  # so much better to just have each pipeline independently within the switch/case

  basisDF_ref <- data.frame(time_orig = sort(unique(na.omit(timeVar))))

  if(is.character(basis_calc_fun)){

    timeOut <- switch(basis_calc_fun
                      ,gaussian = {
                        calc_fun <- function(...,width){dnorm(...,sd=width/constAdj )}

                        constAdj <- sqrt(2 * log(2)) /2
                        lowerOffset <- signif(mean(diff(timeVar)),2)

                        basisCenters <- seq(min(basisDF_ref$time_orig) - (basisDens/2+lowerOffset)
                                            ,max(basisDF_ref$time_orig) + basisDens/2
                                            ,basisDens)
                        basisNamePrec <- 3 - nchar(signif(max(basisCenters) - min(basisCenters),3))

                        for(curBasisCenter in basisCenters){
                          basisDF_ref[,paste0('basis_cen_',round(curBasisCenter,basisNamePrec))] <-
                            calc_fun(basisDF_ref$time_orig, curBasisCenter, width = basisDens/2)
                        }

                        colnames(basisDF_ref) <- gsub('-','M', colnames(basisDF_ref),fixed = T)

                        basisDF_ref[,2:ncol(basisDF_ref)] <-
                          basisDF_ref[,2:ncol(basisDF_ref)] / rowSums(basisDF_ref[,2:ncol(basisDF_ref)])



                      }
                      ,bspline ={
                        library(fda)
                        nBases <- floor(length(timeVar)/basisDens) + 1

                        bsplineObj <- create.bspline.basis(rangeval = c(min(timeVar,na.rm=T)
                                                                        ,max(timeVar,na.rm = T))
                                                           ,nbasis = nBases)
                        basisDF_ref <- cbind(basisDF_ref,eval.basis(basisDF_ref$time_orig,bsplineObj) )

                        ## find the centers
                        basisCenters <- c()
                        for(curBasis in 2:ncol(basisDF_ref)){
                          curBasisMode <- basisDF_ref$time_orig[which.max(basisDF_ref[,curBasis])]
                          basisCenters <- c(basisCenters
                                            ,curBasisMode)
                          colnames(basisDF_ref)[curBasis] <- paste0('basis_mode_',curBasisMode)

                        }

                      }
    )
  }else{
    error('basis calculation function not one of available options')
  }

  timeOut <- data.frame(order_orig = 1:length(timeVar),time_orig = timeVar)

  timeOut <- merge(timeOut,  basisDF_ref, by='time_orig')

  timeOut <- timeOut[order(timeOut$order_orig),]

  timeOut$order_orig <- NULL

  attr(timeOut,'basis_modes') <- basisCenters

  return(timeOut)
}
