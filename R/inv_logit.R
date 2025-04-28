#' Inverse logistic transformation
#' 
#' This is simply `stats::plogis`, renamed for compatibility between R and 
#' Stan. `brms` previously included this, but it was removed from that package.
#'
#' @param q 
#'
#' @export
#'
#' @examples
#' 
#' all.equal(inv_logit(2),plogis(2))
#' 
inv_logit <- function(q){
  plogis(q)
}
