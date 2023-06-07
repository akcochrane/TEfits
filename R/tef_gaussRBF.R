#' From a numeric vector, get a set of weight vectors corresponding to normalized Gaussian radial basis functions
#'
#' Given a numeric vector (e.g., time points) and the number of quantiles to divide it into (e.g., quartiles),
#' define a set of weight vectors. In the case of quartiles, for example, the most weight for the vectors
#' is centered on the the 0, .25, .5, .75, and 1 quantiles of the input vector. In addition, two extra
#' weight vectors are included that extrapolate beyond the original data (in this case, they would be the
#' "-.25 quantile" and the "1.25 quantile"). The weight vectors are calculated using a Gaussian radial
#' basis function (https://en.wikipedia.org/wiki/Radial_basis_function), with a width approximately defined
#' to have the values with half the maximum weight of the vector be half of the inter-peak width.
#' After the basis functions are defined, they are normalized for every value
#' of the original vectors (i.e., \code{weight_i/sum(weights)}).
#'
#' @param x numeric vector, e.g., time points
#' @param quantiles
#'
#' @export
#'
#' @examples
#' tim <- 1:200
#' tim_RBFs <- tef_gaussRBF(tim)
#' pairs(tim_RBFs)
#'
tef_gaussRBF <- function(x,quantiles = 4){

  get_gaussRBF <- function(kernScale,x){

  d <- data.frame(dat_original = x)

  basisCenters <- seq(min(x,na.rm = T),max(x,na.rm = T),length = quantiles + 1)

  basisWidth <- median(diff(basisCenters))

  basisCenters <- c(basisCenters[1] - basisWidth,basisCenters)
  basisCenters <- c(basisCenters, basisCenters[length(basisCenters)] + basisWidth)


  for(curCenter in 1:length(basisCenters)){
  # # with the Gaussian radial basis function, per Wikipedia:
  d[,paste0('basis_',curCenter - 1)] <- exp(-(((basisCenters[curCenter] - x) * kernScale ) / basisWidth)^2)

  }

  return(d)
  }

  if(F){ #not currently implemented
  sd_gaussRBF <- function(kernScale){

    d <- get_gaussRBF(kernScale,x=x)

  basisSum <- apply(d[,2:ncol(d)],1,sum)
  # plot(x,basisSum,'l') # not constant
 sd(basisSum)

  }

  bestKern <- optimize(sd_gaussRBF,lower = 1E-2,upper = 4 )
  d <- get_gaussRBF(bestKern$minimum,x)
  }

  d <- get_gaussRBF(2,x)


  d[,2:ncol(d)] <-
    t( apply(d[,2:ncol(d)]
             ,1,FUN = function(x){
               x / sum(x) # scales nicely between 0 and about .96
               # exp(x)/sum(exp(x)) # softmax function ; ends up scaling between about .15 and .35
             }) )

  # basisSum_swept <- apply(d[,2:ncol(d)],1,sum)
  # plot(x,basisSum_swept) # normalized
  # psych::pairs.panels(d)

  attr(d,'kernel_hwhm') <-
    d[which.min(abs(d$basis_1 - mean(c(max(d$basis_1),min(d$basis_1))) )),'dat_original']

  return(d)
}
