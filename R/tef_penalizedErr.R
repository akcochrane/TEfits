#' Penalize error for being close to a boundary
#'
#' Formally, `pErr=Err/dbeta(par,dbeta_penalty,dbeta_penalty)`, where
#' par is the rate parameter normalized between 0 [lower bound] and 1 [upper bound]
#'
#' @param boundedPar Value of the parameter that has bounds.
#' @param errPar Value of the error to be penalized.
#' @param loBound Lower parameter boundary.
#' @param upBound Upper parameter boundary.
#' @param dbeta_penalty The multiplicative error penalty at approximately 5.3 percent away from the boundary. Error increases with increasing proximity to a bound.
#'
#' @export
#'
tef_penalizedErr <-  function(boundedPar,errPar,loBound,upBound,dbeta_penalty=1.001){
  boundedPar <- (boundedPar-loBound)/(upBound-loBound)
  p_err <- errPar/dbeta(boundedPar,dbeta_penalty,dbeta_penalty)
  return(p_err)
}
